(in-package #:cl-worktree)

(define-condition worktree-error (error)
  ((operation :initarg :operation :reader failure-operation
              :documentation "The failed public operation or lifecycle transition.")
   (detail :initarg :detail :reader failure-detail
           :documentation "Concrete diagnostic context, including Git stderr when available."))
  (:report (lambda (condition stream)
             (format stream "~A: ~A" (failure-operation condition)
                     (failure-detail condition))))
  (:documentation "A typed lifecycle or integration failure."))

(define-condition command-failure (worktree-error)
  ((command :initarg :command :reader failure-command
            :documentation "The attempted Git subcommand argument list.")
   (exit-code :initarg :exit-code :reader failure-exit-code
              :documentation "The unsuccessful integer process exit status."))
  (:documentation "The injected runner returned an unsuccessful Git status."))

(define-condition isolation-unavailable (worktree-error) ()
  (:documentation "The source is not an available Git worktree."))

(define-condition dirty-workspace (worktree-error) ()
  (:documentation "An operation requires a clean index and working directory."))

(define-condition ownership-error (worktree-error) ()
  (:documentation "Identity or repository ownership could not be verified."))

(define-condition worktree-identity-mismatch (ownership-error) ()
  (:documentation "The expected durable identity did not select a unique owned handle."))

(define-condition cleanup-failure (worktree-error) ()
  (:documentation "Cleanup failed; retained metadata allows inspection and retry."))

(define-condition integration-conflict (worktree-error)
  ((paths :initarg :paths :reader conflict-paths
          :documentation "Unmerged paths, or NIL for a failed non-mutating patch check."))
  (:documentation "Integration failed, possibly leaving explicit conflict state."))

(defstruct (manager (:constructor %make-manager))
  "Command runner and a private durable registry directory."
  runner registry)

(defstruct worktree
  "A detached worktree identity; registry records persist across process restarts."
  id owner source path baseline git-directory (state :active))

(defstruct artifact
  "A portable baseline-relative change artifact with explicit provenance."
  kind baseline tip content paths owner source common-git-directory isolated-path
  worktree-id patch patch-reference)

(defun make-manager (&key runner registry)
  "RUNNER accepts argv and :DIRECTORY/:INPUT, returning stdout, stderr, exit code.
REGISTRY must be a private directory. Callers serialize operations per repository."
  (check-type runner function)
  (unless registry
    (error 'worktree-error :operation :manager :detail "A private registry is required"))
  (let ((path (uiop:ensure-directory-pathname
               (merge-pathnames registry (uiop:getcwd)))))
    (ensure-directories-exist (merge-pathnames "disabled-hooks/probe" path))
    (%make-manager :runner runner :registry path)))

(defun %git-base-argv (&optional (hooks-path ""))
  "Build the common Git prefix without consulting any registry or process state."
  (list "git" "--no-pager" "--no-optional-locks"
        "-c" (concatenate 'string "core.hooksPath=" hooks-path)
        "-c" "core.fsmonitor=false" "-c" "gc.auto=0" "-c" "maintenance.auto=false"))

(defun git-read-argv (arguments)
  "Build argv for a caller-selected read-only Git metadata query without a manager."
  (append (%git-base-argv) arguments))

(defun %git-prefix (manager)
  "Disable externally executable Git features and automatic background mutation."
  (append (%git-base-argv (namestring (merge-pathnames "disabled-hooks/"
                                                      (manager-registry manager))))
          '("-c" "core.sparseCheckout=false" "-c" "commit.gpgsign=false"
            "-c" "tag.gpgsign=false" "-c" "rerere.enabled=false" "-c" "submodule.recurse=false")))

(defun %invoke (manager directory argv &key input)
  "Validate the runner result and classify execution failures without an exit status."
  (handler-case
      (multiple-value-bind (out err code)
          (funcall (manager-runner manager) argv :directory directory :input input)
        (unless (and (stringp out) (stringp err) (integerp code))
          (error 'worktree-error :operation :runner :detail "Invalid runner result"))
        (values out err code))
    (worktree-error (condition) (error condition))
    (error (condition)
      (error 'worktree-error :operation :runner :detail (princ-to-string condition)))))

(defun %filter-options (manager directory)
  "Disable configured external clean, smudge and process filters for each command."
  (multiple-value-bind (out err code)
      (%invoke manager directory
               (append (%git-prefix manager)
                       '("config" "--null" "--name-only" "--get-regexp"
                         "^filter\\..*\\.(clean|smudge|process|required)$")))
    (unless (member code '(0 1))
      (error 'command-failure :operation :configuration :detail err
             :command '("config" "--get-regexp") :exit-code code))
    (loop for key in (%split-zero out)
          append (list "-c" (concatenate 'string key "="
                                        (if (uiop:string-suffix-p ".required" key)
                                            "false" ""))))))

(defun %run (manager directory arguments &key input (check t))
  "Invoke Git with explicit safe per-command policy, without a shell."
  (multiple-value-bind (out err code)
      (%invoke manager directory
               (append (%git-prefix manager) (%filter-options manager directory) arguments)
               :input input)
    (when (and check (not (zerop code)))
      (error 'command-failure :operation (first arguments) :detail err
             :command arguments :exit-code code))
    (values out err code)))

(defun %trim (string)
  "Remove Git's terminating newline."
  (string-right-trim '(#\Newline #\Return) string))

(defun %git-directory (manager path)
  "Resolve a worktree's private Git directory."
  (%trim (%run manager path '("rev-parse" "--absolute-git-dir"))))

(defun %source (manager source)
  "Resolve the source root or report unavailability, including missing directories."
  (handler-case
      (multiple-value-bind (out err code)
          (%run manager source '("rev-parse" "--show-toplevel") :check nil)
        (unless (zerop code)
          (error 'isolation-unavailable :operation :source :detail err))
        (%trim out))
    (isolation-unavailable (condition) (error condition))
    (error (condition)
      (error 'isolation-unavailable :operation :source :detail (princ-to-string condition)))))

(defun %clean (manager path)
  "Require a clean index, tracked files, and untracked files."
  (unless (zerop (length (%run manager path '("status" "--porcelain" "-z"))))
    (error 'dirty-workspace :operation :clean :detail path)))

(defun %revision (manager path revision)
  "Resolve an unambiguous commit without permitting option injection."
  (%trim (%run manager path
               (list "rev-parse" "--verify" "--end-of-options"
                     (concatenate 'string revision "^{commit}")))))

(defun %record-path (manager id)
  "Return an identity's private registry record pathname."
  (unless (and (stringp id) (plusp (length id))
               (every (lambda (character)
                        (or (find character "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
                            (char= character #\-)))
                      id))
    (error 'ownership-error :operation :identity :detail "Invalid worktree identity"))
  (merge-pathnames (make-pathname :name id :type "sexp")
                   (manager-registry manager)))

(defun %marker-path (worktree)
  "Return the identity marker stored in the private Git directory."
  (merge-pathnames "cl-worktree-owner.sexp"
                   (uiop:ensure-directory-pathname (worktree-git-directory worktree))))

(defun %data (worktree)
  "Produce a portable record."
  (list :id (worktree-id worktree) :owner (worktree-owner worktree)
        :source (worktree-source worktree) :path (worktree-path worktree)
        :baseline      (worktree-baseline worktree)
        :git-directory (worktree-git-directory worktree)
        :state         (worktree-state worktree)))

(defun %write-record (path data)
  "Publish readable metadata atomically."
  (let ((temporary (make-pathname :type "new" :defaults path)))
    (with-open-file (stream temporary :direction :output :if-exists :supersede)
      (with-standard-io-syntax (write data :stream stream)))
    (uiop:rename-file-overwriting-target temporary path)))

(defun %read-record (path)
  "Read exactly one metadata form with reader evaluation disabled."
  (handler-case
      (with-open-file (stream path)
        (with-standard-io-syntax
          (let* ((*read-eval* nil)
                 (end         (gensym "END"))
                 (data        (read stream)))
            (unless (eq end (read stream nil end))
              (error 'ownership-error :operation :metadata :detail "Trailing metadata"))
            data)))
    (ownership-error (condition) (error condition))
    (error (condition)
      (error 'ownership-error :operation :metadata :detail (princ-to-string condition)))))

(defun %repository-directory (path)
  "Canonicalize an existing absolute native directory, or return NIL."
  (handler-case
      (let ((directory (uiop:ensure-directory-pathname
                        (if (pathnamep path) path (uiop:parse-native-namestring path)))))
        (when (uiop:absolute-pathname-p directory)
          (let ((existing (uiop:directory-exists-p directory)))
            (when existing (uiop:native-namestring (truename existing))))))
    (file-error () nil)))

(defun %admin-path-kind (path)
  "Recognize directory and file paths with the default filesystem adapter."
  (cond ((uiop:directory-exists-p (uiop:ensure-directory-pathname path))
         :directory)
        ((uiop:file-exists-p path)
         :file)))

(defun %admin-error (detail)
  "Report incomplete or malformed exact-checkout metadata."
  (error 'ownership-error :operation :checkout-admin-roots :detail detail))

(defun %admin-pointer (file directory &key (prefix ""))
  "Read one nonempty native path from a bounded Git pointer file."
  (with-open-file (stream file)
    (let* ((raw  (with-output-to-string (text)
                   (loop for character = (read-char stream nil nil)
                         repeat 65536
                         while (and character (char/= character #\Newline))
                         do (write-char character text)
                         finally (when (and character (char/= character #\Newline))
                                   (%admin-error "Git administrative pointer is too long")))))
           (line (if (uiop:string-suffix-p (string #\Return) raw)
                     (subseq raw 0 (1- (length raw)))
                     raw)))
      (unless (and (uiop:string-prefix-p prefix line) (> (length line) (length prefix))
                   (not (find #\Null line)) (not (find #\Return line))
                   (not (read-char stream nil nil)))
        (%admin-error "Malformed Git administrative pointer"))
      (merge-pathnames (uiop:parse-native-namestring (subseq line (length prefix))) directory))))

(defun %admin-canonical-path (path kind &key path-kind canonicalize)
  "Require an existing canonical native pathname of KIND."
  (let* ((path   (if (eq kind :directory) (uiop:ensure-directory-pathname path) path))
         (result (and (eq kind (funcall path-kind path)) (funcall canonicalize path))))
    (unless (and (pathnamep result) (uiop:absolute-pathname-p result)
                 (not (typep result 'logical-pathname)) (not (wild-pathname-p result))
                 (not (intersection (pathname-directory result) '(:up :back)))
                 (eq (not (null (uiop:directory-pathname-p result))) (eq kind :directory))
                 (eq kind (funcall path-kind result)))
      (%admin-error "Git administrative path is missing or not canonical"))
    result))

(defun checkout-admin-roots (checkout &key (path-kind #'%admin-path-kind) (canonicalize #'truename))
  "Return CHECKOUT's canonical private/common Git directories as distinct pathnames.
Inspect only its .git entry without a subprocess. Return NIL when that entry
is absent; signal OWNERSHIP-ERROR for invalid or incomplete metadata. PATH-KIND
returns :DIRECTORY, :FILE, NIL for absence, or another value for unsupported kinds.
CANONICALIZE returns an existing absolute native pathname, including for files."
  (handler-case
      (flet ((canonical (path kind)
               (%admin-canonical-path path kind :path-kind path-kind :canonicalize canonicalize)))
        (let* ((directory (canonical (pathname checkout) :directory))
               (entry     (merge-pathnames ".git" directory))
               (kind      (funcall path-kind entry)))
          (when kind
            (let* ((private (canonical
                             (case kind
                               (:directory
                                entry)
                               (:file
                                (%admin-pointer entry directory :prefix "gitdir: "))
                               (otherwise
                                (%admin-error "Invalid .git entry kind")))
                             :directory))
                   (pointer (merge-pathnames "commondir" private))
                   (common  (case (funcall path-kind pointer)
                              ((nil)
                               private)
                              (:file
                               (let ((backlink (merge-pathnames "gitdir" private)))
                                 (unless (and (eq kind :file) (eq (funcall path-kind backlink) :file)
                                              (equal (canonical (%admin-pointer backlink private) :file)
                                                     (canonical entry :file)))
                                   (%admin-error "Linked Git directory does not identify this checkout")))
                               (canonical (%admin-pointer pointer private) :directory))
                              (otherwise
                               (%admin-error "Invalid commondir entry kind")))))
              (unless (and (eq :file (funcall path-kind (merge-pathnames "HEAD" private)))
                           (eq :directory (funcall path-kind (merge-pathnames "objects/" common)))
                           (eq :directory (funcall path-kind (merge-pathnames "refs/" common))))
                (%admin-error "Incomplete Git administrative directories"))
              (if (equal private common) (list private) (list private common))))))
    (ownership-error (condition) (error condition))
    (error (condition) (%admin-error (princ-to-string condition)))))

(defun %repository-path (directory arguments git-output &key canonicalize)
  "Read a Git path, removing only the protocol's final newline."
  (let ((output (funcall git-output directory arguments)))
    (when (and output (> (length output) 1)
               (char= (char output (1- (length output))) #\Newline))
      (funcall canonicalize (subseq output 0 (1- (length output)))))))

(defun describe-repository (directory git-output &key (canonicalize #'%repository-directory))
  "Return DIRECTORY's local Git identity as (:COMMON ... :PRIVATE ... :ROOT ...), or NIL.
GIT-OUTPUT takes a directory and fixed Git subcommand arguments and returns
successful stdout or NIL; the caller controls the process policy. CANONICALIZE
returns a canonical name only for existing absolute directories. A checkout's
private directory can differ from its common directory, and a bare repository
has no checkout root. No registry or Git state is written."
  (when (funcall canonicalize directory)
    (let ((common (%repository-path directory '("rev-parse" "--path-format=absolute" "--git-common-dir")
                                    git-output :canonicalize canonicalize)))
      (when common
        (list :common  common
              :private (%repository-path directory '("rev-parse" "--absolute-git-dir") git-output :canonicalize canonicalize)
              :root    (%repository-path directory '("rev-parse" "--show-toplevel") git-output :canonicalize canonicalize))))))

(defun repository-primary-checkout (description)
  "Return the observed primary checkout root in DESCRIPTION, if any."
  (let ((common (getf description :common)))
    (when (and common (equal common (getf description :private)))
      (getf description :root))))

(defun %worktree-field-path (field)
  "Return the literal path from a porcelain worktree field, if present."
  (when (and field (uiop:string-prefix-p "worktree " field))
    (subseq field 9)))

(defun repository-origin (directory description git-output
                          &key (canonicalize #'%repository-directory) describe)
  "Return the verified primary path for DESCRIPTION, or NIL.
DESCRIBE may supply cached descriptions of other directories; by default query
with GIT-OUTPUT and CANONICALIZE. The first worktree record is only a candidate:
verify its administrative identity and checkout root, or its bare identity."
  (or (repository-primary-checkout description)
      (let ((common (getf description :common)))
        (when common
          (let* ((output (funcall git-output directory '("worktree" "list" "--porcelain" "-z")))
                 (end    (and output (search '(#\Null #\Null) output)))
                 (fields (and end (uiop:split-string (subseq output 0 end) :separator '(#\Null))))
                 (path   (%worktree-field-path (first fields)))
                 (path   (and path (funcall canonicalize path))))
            (when path
              (if (member "bare" fields :test #'string=)
                  (and (equal path common) path)
                  (let ((candidate (if describe
                                       (funcall describe path)
                                       (describe-repository path git-output :canonicalize canonicalize))))
                    (when (and (equal common (getf candidate :common))
                               (equal path (repository-primary-checkout candidate)))
                      path)))))))))

(defun %registered-paths (manager source)
  "Read registered paths using Git's NUL-delimited porcelain format."
  (loop for field in (%split-zero
                      (%run manager source '("worktree" "list" "--porcelain" "-z")))
        for path = (%worktree-field-path field)
        when path collect path))

(defun %owned (manager worktree owner &key absent-ok)
  "Verify registry, owner, registered path and private Git marker before mutation.
Return :RETIRED only when both the worktree and its Git metadata are absent."
  (let* ((data       (%data worktree))
         (record     (%record-path manager (worktree-id worktree)))
         (record-p   (probe-file record))
         (marker     (%marker-path worktree))
         (registered (member (worktree-path worktree)
                             (%registered-paths manager (worktree-source worktree))
                             :test #'equal)))
    (unless (and (equal owner (worktree-owner worktree))
                 (or (not record-p) (equal data (%read-record record))))
      (error 'ownership-error :operation :ownership :detail "Registry or owner mismatch"))
    (cond
      ((probe-file (worktree-path worktree))
       (unless (and registered
                    (equal (%git-directory manager (worktree-path worktree))
                           (worktree-git-directory worktree))
                    (equal data (%read-record marker)))
         (error 'ownership-error :operation :ownership :detail "Git marker mismatch"))
       :active)
      ((not absent-ok)
       (error 'ownership-error :operation :ownership :detail "Worktree is absent"))
      (registered
       (unless (equal data (%read-record marker))
         (error 'ownership-error :operation :ownership :detail "Missing orphan marker"))
       :orphan)
      ((and record-p (not (probe-file marker))
            (not (probe-file (worktree-git-directory worktree))))
       :retired)
      (t
       (error 'ownership-error :operation :ownership :detail "Unregistered Git metadata")))))

(defun %creation-reason (worktree)
  "Identify this invocation in Git's atomic creation lock."
  (format nil "cl-worktree ~A" (worktree-id worktree)))

(defun %creation-reservation (manager worktree owner)
  "Require an unchanged pending handle and its durable reservation."
  (unless (and (eq (worktree-state worktree) :creating)
               (equal owner (worktree-owner worktree))
               (equal (%data worktree)
                      (%read-record (%record-path manager (worktree-id worktree)))))
    (error 'ownership-error :operation :creation :detail "Creation reservation mismatch")))

(defun %creation-directory (manager worktree)
  "Find private Git metadata by its backlink, including absent checkouts."
  (let* ((common   (%trim (%run manager (worktree-source worktree)
                               '("rev-parse" "--path-format=absolute" "--git-common-dir"))))
         (backlink (namestring (merge-pathnames ".git"
                                               (uiop:ensure-directory-pathname
                                                (worktree-path worktree)))))
         (matches
           (loop for directory in (uiop:subdirectories
                                   (merge-pathnames "worktrees/"
                                                    (uiop:ensure-directory-pathname common)))
                 for file = (merge-pathnames "gitdir" directory)
                 when (and (probe-file file)
                           (equal backlink (%trim (uiop:read-file-string file))))
                   collect (string-right-trim '(#\/ #\\) (namestring directory)))))
    (when (rest matches)
      (error 'ownership-error :operation :creation :detail "Ambiguous Git metadata"))
    (first matches)))

(defun %creation-proof (worktree directory)
  "Require Git's invocation-specific lock before adopting pending metadata."
  (let ((lock (merge-pathnames "locked" (uiop:ensure-directory-pathname directory))))
    (unless (and (probe-file lock)
                 (equal (%creation-reason worktree) (%trim (uiop:read-file-string lock))))
      (error 'ownership-error :operation :creation :detail "Missing or foreign creation proof"))))

(defun %unlock-creation (manager worktree)
  "Release only this invocation's creation lock after ownership publication."
  (let ((lock (merge-pathnames "locked"
                              (uiop:ensure-directory-pathname (worktree-git-directory worktree)))))
    (when (probe-file lock)
      (%creation-proof worktree (worktree-git-directory worktree))
      (%run manager (worktree-source worktree)
            (list "worktree" "unlock" "--" (worktree-path worktree))))))

(defun %publish-creation (manager worktree directory)
  "Publish proven creation ownership before releasing Git's creation lock."
  (%creation-proof worktree directory)
  (let ((active (copy-worktree worktree)))
    (setf (worktree-git-directory active) directory
          (worktree-state active) :active)
    (let ((marker (%marker-path active)))
      (when (and (probe-file marker) (not (equal (%data active) (%read-record marker))))
        (error 'ownership-error :operation :recovery :detail "Foreign ownership marker"))
      (%write-record marker (%data active))
      (%write-record (%record-path manager (worktree-id active)) (%data active)))
    active))

(defun create-worktree (manager &key source path baseline owner (dirty-policy :reject)
                                 reserved-function)
  "Reserve durable ownership before Git mutation, then create a detached checkout.
RESERVED-FUNCTION receives the pending handle before Git runs. Interrupted creation
retains its registry record for explicit RECOVER-WORKTREE or cleanup."
  (check-type owner string)
  (unless (member dirty-policy '(:reject :ignore))
    (error 'worktree-error :operation :create :detail "Unknown dirty-source policy"))
  (let* ((root        (%source manager source))
         (destination (string-right-trim '(#\/ #\\)
                                         (namestring (merge-pathnames path (uiop:getcwd)))))
         (revision    (%revision manager root baseline))
         (id          (loop for candidate = (format nil "~36R-~36R" (get-universal-time)
                                                   (random (expt 2 80)))
                            unless (probe-file (%record-path manager candidate)) return candidate)))
    (when (or (probe-file destination)
              (find destination (discover-worktrees manager) :key #'worktree-path :test #'equal))
      (error 'ownership-error :operation :create :detail "Destination already exists or is reserved"))
    (when (eq dirty-policy :reject)
      (restart-case (%clean manager root)
        (use-committed-baseline ()
          :report "Use the committed baseline, excluding all dirty source changes."
          nil)))
    (let ((worktree (make-worktree :id id :owner owner :source root :path destination
                                   :baseline revision :state :creating)))
      (%write-record (%record-path manager id) (%data worktree))
      (when reserved-function (funcall reserved-function worktree))
      (%creation-reservation manager worktree owner)
      (%run manager root (list "worktree" "add" "--detach" "--lock" "--reason"
                               (%creation-reason worktree) "--" destination revision))
      (recover-worktree manager worktree :owner owner))))

(defun recover-worktree (manager worktree &key owner)
  "Complete interrupted creation with the reservation and Git's creation lock.
Require invocation-specific proof before publishing ownership. Ambiguous or foreign
metadata remains reserved for inspection; recovery never claims an unmarked checkout."
  (unless (and (eq (worktree-state worktree) :creating)
               (probe-file (worktree-path worktree)))
    (return-from recover-worktree worktree))
  (%creation-reservation manager worktree owner)
  (unless (and (member (worktree-path worktree)
                       (%registered-paths manager (worktree-source worktree)) :test #'equal)
               (equal (worktree-path worktree) (%source manager (worktree-path worktree)))
               (equal (worktree-baseline worktree) (%revision manager (worktree-path worktree) "HEAD")))
    (error 'ownership-error :operation :recovery :detail "Creation reservation or checkout mismatch"))
  (let ((directory (%git-directory manager (worktree-path worktree))))
    (unless (equal directory (%creation-directory manager worktree))
      (error 'ownership-error :operation :recovery :detail "Foreign Git directory"))
    (let ((active (%publish-creation manager worktree directory)))
      (%unlock-creation manager active)
      active)))

(defun discover-worktrees (manager &key owner source)
  "Return two values: owned handles and absent orphan handles.
With SOURCE, also discover Git markers whose registry publication was interrupted.
Discovery is read-only and never claims unmarked or foreign worktrees."
  (let ((trees (loop for file in (uiop:directory-files (manager-registry manager) "*.sexp")
                    collect (handler-case (apply #'make-worktree (%read-record file))
                              (error (condition)
                                (error 'ownership-error
                                       :operation :discovery
                                       :detail    (princ-to-string condition)))))))
    (when source
      (let* ((root     (%source manager source))
             (metadata (%trim (%run manager root
                                    '("rev-parse" "--path-format=absolute"
                                      "--git-common-dir")))))
        (dolist (directory (uiop:subdirectories
                            (merge-pathnames "worktrees/"
                                             (uiop:ensure-directory-pathname metadata))))
          (let ((marker (merge-pathnames "cl-worktree-owner.sexp" directory)))
            (when (probe-file marker)
              (let ((tree (apply #'make-worktree (%read-record marker))))
                (when (and (equal root (worktree-source tree))
                           (not (find (worktree-id tree) trees
                                      :key #'worktree-id :test #'equal)))
                  (push tree trees))))))))
    (let ((selected (remove-if-not
                     (lambda (tree) (or (null owner) (equal owner (worktree-owner tree))))
                     trees)))
      (values selected (remove-if (lambda (tree) (probe-file (worktree-path tree))) selected)))))

(defun recover-worktree-by-identity (manager &key owner source id baseline path)
  "Recover only a discovered handle matching every retained identity field.
OWNER, SOURCE, ID, BASELINE and PATH are exact strings as recorded by the handle.
Match both present and absent records before any recovery effects."
  (unless (every (lambda (value) (and (stringp value) (plusp (length value))))
                 (list owner source id baseline path))
    (error 'worktree-identity-mismatch :operation :recovery :detail "Incomplete expected worktree identity"))
  (multiple-value-bind (owned absent)
      (discover-worktrees manager :owner owner :source source)
    (flet ((matches-p (tree)
             (and (equal owner (worktree-owner tree)) (equal source (worktree-source tree))
                  (equal id (worktree-id tree)) (equal baseline (worktree-baseline tree))
                  (equal path (worktree-path tree)))))
      (let ((matches (remove-if-not #'matches-p (union owned absent :test #'eq))))
        (unless (= (length matches) 1)
          (error 'worktree-identity-mismatch :operation :recovery :detail "Owned worktree identity unavailable or changed"))
        (recover-worktree manager (first matches) :owner owner)))))

(defun cleanup-worktree (manager worktree &key owner discard-dirty)
  "Remove verified ownership, returning T or NIL on RETAIN-WORKTREE.
Interrupted creations retain their reservation until recovery or explicit cleanup.
DISCARD-DIRTY permits forced removal; RETRY-CLEANUP revalidates ownership."
  (restart-case
      (progn
        (when (eq (worktree-state worktree) :creating)
          (%creation-reservation manager worktree owner)
          (unless (probe-file (worktree-path worktree))
            (let ((directory  (%creation-directory manager worktree))
                  (registered (member (worktree-path worktree)
                                      (%registered-paths manager (worktree-source worktree))
                                      :test #'equal)))
              (when (or directory registered)
                (unless (and directory registered)
                  (error 'ownership-error :operation :cleanup :detail "Unmatched pending Git metadata"))
                (let* ((active  (%publish-creation manager worktree directory))
                       (removed (cleanup-worktree manager active :owner       owner
                                                               :discard-dirty discard-dirty)))
                  (when removed (setf (worktree-state worktree) :removed))
                  (return-from cleanup-worktree removed))))
            (delete-file (%record-path manager (worktree-id worktree)))
            (setf (worktree-state worktree) :removed)
            (return-from cleanup-worktree t))
          (setf worktree (recover-worktree manager worktree :owner owner)))
        (let ((status (%owned manager worktree owner :absent-ok t)))
          (handler-case
              (progn
                (unless (eq status :retired)
                  (when (probe-file (worktree-path worktree))
                    (unless discard-dirty (%clean manager (worktree-path worktree))))
                  (%unlock-creation manager worktree)
                  (%run manager (worktree-source worktree)
                        (append '("worktree" "remove") (when discard-dirty '("--force"))
                                (list "--" (worktree-path worktree)))))
                (let ((record (%record-path manager (worktree-id worktree))))
                  (when (probe-file record) (delete-file record)))
                (setf (worktree-state worktree) :removed)
                t)
            (dirty-workspace (condition) (error condition))
            (error (condition)
              (error 'cleanup-failure :operation :cleanup :detail (princ-to-string condition))))))
    (retain-worktree ()
      :report "Retain the owned worktree and its registry without discarding edits."
      nil)
    (retry-cleanup ()
      :report "Retry cleanup after repairing the failure, revalidating ownership."
      (cleanup-worktree manager worktree :owner owner :discard-dirty discard-dirty))))

(defun %split-zero (text)
  "Split Git's NUL-delimited names, preserving spaces and newlines."
  (remove "" (uiop:split-string text :separator '(#\Null)) :test #'equal))

(defun %tip (manager worktree)
  "Resolve the isolated current commit."
  (%revision manager (worktree-path worktree) "HEAD"))

(defun %artifact (manager tree &key kind tip content paths patch)
  "Attach complete provenance to an extracted terminal artifact."
  (make-artifact :kind kind :baseline (worktree-baseline tree) :tip tip
                 :content content :paths paths :owner (worktree-owner tree)
                 :source (worktree-source tree) :isolated-path (worktree-path tree)
                 :worktree-id (worktree-id tree) :patch patch
                 :common-git-directory
                 (%trim (%run manager (worktree-path tree)
                              '("rev-parse" "--path-format=absolute" "--git-common-dir")))))

(defun artifact->plist (artifact &key patch-reference)
  "Return versioned portable data suitable for persistence or JSON conversion.
PATCH-REFERENCE optionally identifies a separately secured patch; content is retained."
  (list :version 1 :kind (ecase (artifact-kind artifact)
                          (:patch
                           "patch")
                          (:commit-range
                           "commit-range"))
        :worktree-id (artifact-worktree-id artifact) :owner (artifact-owner artifact)
        :source               (artifact-source artifact)
        :common-git-directory (artifact-common-git-directory artifact)
        :isolated-path        (artifact-isolated-path artifact)
        :baseline (artifact-baseline artifact) :tip (artifact-tip artifact)
        :patch                (artifact-patch artifact)
        :patch-reference      (or patch-reference (artifact-patch-reference artifact))
        :commits              (when (eq (artifact-kind artifact) :commit-range) (artifact-content artifact))
        :touched-paths        (artifact-paths artifact)))

(defun %commit-id-p (value)
  "Recognize a full SHA-1 or SHA-256 commit identity."
  (and (stringp value) (member (length value) '(40 64))
       (every (lambda (character) (digit-char-p character 16)) value) t))

(defun plist->artifact (data)
  "Validate and restore version-one portable terminal artifact data."
  (handler-case
      (progn
        (unless (and (listp data) (evenp (length data))
                     (= 1 (getf data :version))
                     (member (getf data :kind) '("patch" "commit-range") :test #'equal)
                     (every (lambda (key) (stringp (getf data key)))
                            '(:worktree-id :owner :source :common-git-directory :isolated-path :patch))
                     (%commit-id-p (getf data :baseline)) (%commit-id-p (getf data :tip))
                     (listp (getf data :commits)) (every #'%commit-id-p (getf data :commits))
                     (listp (getf data :touched-paths)) (every #'stringp (getf data :touched-paths))
                     (or (null (getf data :patch-reference)) (stringp (getf data :patch-reference))))
          (error 'worktree-error :operation :artifact :detail "Invalid terminal artifact"))
        (let ((kind (if (equal "patch" (getf data :kind)) :patch :commit-range)))
          (make-artifact :kind kind :baseline (getf data :baseline) :tip (getf data :tip)
                         :worktree-id (getf data :worktree-id) :owner (getf data :owner)
                         :source               (getf data :source)
                         :common-git-directory (getf data :common-git-directory)
                         :isolated-path        (getf data :isolated-path)
                         :patch (getf data :patch) :patch-reference (getf data :patch-reference)
                         :paths                (getf data :touched-paths)
                         :content              (if (eq kind :patch) (getf data :patch) (getf data :commits)))))
    (worktree-error (condition) (error condition))
    (error (condition)
      (error 'worktree-error :operation :artifact :detail (princ-to-string condition)))))

(defun extract-patch (manager worktree &key owner)
  "Return a binary patch of committed, staged and unstaged tracked changes.
Untracked files are rejected: stage new deliverable files before extraction."
  (%owned manager worktree owner)
  (let ((path (worktree-path worktree)) (base (worktree-baseline worktree)))
    (unless (zerop (length (%run manager path
                                '("ls-files" "--others" "--exclude-standard" "-z"))))
      (error 'dirty-workspace
             :operation :extract-patch
             :detail    "Stage untracked files before patch extraction"))
    (when (integration-conflicts manager path)
      (error 'integration-conflict :operation :extract-patch :detail "Unmerged files"
             :paths (integration-conflicts manager path)))
    (let ((patch (%run manager path
                       (list "diff" "--no-ext-diff" "--no-textconv" "--binary" base "--"))))
      (%artifact manager worktree :kind :patch :tip (%tip manager worktree)
                 :content patch :patch patch
                 :paths (%split-zero (%run manager path
                                           (list "diff" "--no-renames" "--name-only" "-z" base "--")))))))

(defun %commits (manager path &key base tip)
  "Return the validated baseline-to-tip sequence, including an empty NIL range."
  (%run manager path (list "merge-base" "--is-ancestor" base tip))
  (remove "" (uiop:split-string
              (%trim (%run manager path (list "rev-list" "--topo-order" "--reverse"
                                             (format nil "~A..~A" base tip) "--")))
              :separator '(#\Newline))
          :test #'equal))

(defun extract-commit-range (manager worktree &key owner)
  "Return ordered commits after BASELINE, requiring a clean descendant tip."
  (%owned manager worktree owner)
  (%clean manager (worktree-path worktree))
  (let* ((path (worktree-path worktree)) (base (worktree-baseline worktree))
         (tip (%tip manager worktree)))
    (%artifact manager worktree :kind :commit-range :tip tip
               :content (%commits manager path :base base :tip tip)
               :patch   (%run manager path
                              (list "diff" "--no-ext-diff" "--no-textconv" "--binary" base tip "--"))
               :paths   (%split-zero (%run manager path
                                           (list "diff" "--no-renames" "--name-only" "-z" base tip "--"))))))

(defun integration-conflicts (manager target)
  "Return unmerged target paths."
  (%split-zero (%run manager target '("diff" "--name-only" "--diff-filter=U" "-z"))))

(defun %integration-ready (manager target)
  "Require clean files and no existing merge, rebase or sequencer operation."
  (%clean manager target)
  (dolist (name '("CHERRY_PICK_HEAD" "MERGE_HEAD" "REVERT_HEAD"
                  "rebase-merge" "rebase-apply" "sequencer"))
    (when (probe-file (%trim (%run manager target
                                  (list "rev-parse" "--path-format=absolute" "--git-path" name))))
      (error 'integration-conflict
             :operation :busy-target
             :detail    (format nil "Existing Git operation: ~A" name)
             :paths     (integration-conflicts manager target))))
  t)

(defun check-patch (manager target patch)
  "Check PATCH applicability without mutation; return T or signal a conflict."
  (%integration-ready manager target)
  (check-type patch string)
  (unless (zerop (length patch))
    (multiple-value-bind (out err code)
        (%run manager target '("apply" "--check" "--index" "-") :input patch :check nil)
      (declare (ignore out))
      (unless (zerop code)
        (error 'integration-conflict :operation :check-patch :detail err :paths nil))))
  t)

(defun apply-patch (manager target patch)
  "Apply PATCH to a clean target index and working tree after checking it."
  (check-patch manager target patch)
  (unless (zerop (length patch))
    (multiple-value-bind (out err code)
        (%run manager target '("apply" "--index" "-") :input patch :check nil)
      (declare (ignore out))
      (unless (zerop code)
        (error 'integration-conflict :operation :apply-patch :detail err
               :paths (integration-conflicts manager target)))))
  t)

(defun apply-commit-range (manager target artifact)
  "Cherry-pick ARTIFACT commits into a clean target. Conflicts are left for inspection.
Commits must be present in the target object database. Caller controls integration."
  (%integration-ready manager target)
  (unless (eq (artifact-kind artifact) :commit-range)
    (error 'worktree-error :operation :apply-range :detail "Expected commit-range artifact"))
  (let* ((base    (%revision manager target (artifact-baseline artifact)))
         (tip     (%revision manager target (artifact-tip artifact)))
         (commits (%commits manager target :base base :tip tip)))
    (unless (equal commits (artifact-content artifact))
      (error 'worktree-error :operation :apply-range :detail "Commit-range content mismatch"))
    (when commits
      (multiple-value-bind (out err code)
          (%run manager target (append '("cherry-pick" "--") commits) :check nil)
        (declare (ignore out))
        (unless (zerop code)
          (error 'integration-conflict :operation :apply-range :detail err
                 :paths (integration-conflicts manager target)))))
    t))

(defun abort-integration (manager target)
  "Abort an in-progress cherry-pick using Git's recorded pre-integration state."
  (%run manager target '("cherry-pick" "--abort"))
  t)
