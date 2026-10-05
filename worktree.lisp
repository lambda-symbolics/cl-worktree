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

(defun %git-prefix (manager)
  "Disable externally executable Git features and automatic background mutation."
  (list "git" "--no-pager" "--no-optional-locks"
        "-c" (format nil "core.hooksPath=~A"
                     (namestring (merge-pathnames "disabled-hooks/"
                                                  (manager-registry manager))))
        "-c" "core.fsmonitor=false" "-c" "gc.auto=0"
        "-c" "maintenance.auto=false" "-c" "core.sparseCheckout=false"
        "-c" "commit.gpgsign=false" "-c" "tag.gpgsign=false"
        "-c" "rerere.enabled=false" "-c" "submodule.recurse=false"))

(defun %invoke (manager directory argv input)
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
                         "^filter\\..*\\.(clean|smudge|process|required)$"))
               nil)
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
               input)
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
        :baseline (worktree-baseline worktree)
        :git-directory (worktree-git-directory worktree)
        :state (worktree-state worktree)))

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
                 (end (gensym "END"))
                 (data (read stream)))
            (unless (eq end (read stream nil end))
              (error 'ownership-error :operation :metadata :detail "Trailing metadata"))
            data)))
    (ownership-error (condition) (error condition))
    (error (condition)
      (error 'ownership-error :operation :metadata :detail (princ-to-string condition)))))

(defun %registered-paths (manager source)
  "Read registered paths using Git's NUL-delimited porcelain format."
  (loop for field in (%split-zero
                     (%run manager source '("worktree" "list" "--porcelain" "-z")))
        when (uiop:string-prefix-p "worktree " field) collect (subseq field 9)))

(defun %owned (manager worktree owner &key absent-ok)
  "Verify registry, owner, registered path and private Git marker before mutation.
Return :RETIRED only when both the worktree and its Git metadata are absent."
  (let* ((data (%data worktree))
         (record (%record-path manager (worktree-id worktree)))
         (record-p (probe-file record))
         (marker (%marker-path worktree))
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

(defun create-worktree (manager &key source path baseline owner (dirty-policy :reject))
  "Create a new detached worktree at BASELINE. :IGNORE excludes source dirt.
Existing paths are never reused. SOURCE is never checked out, reset, or stashed."
  (check-type owner string)
  (unless (member dirty-policy '(:reject :ignore))
    (error 'worktree-error :operation :create :detail "Unknown dirty-source policy"))
  (let* ((root (%source manager source))
         (destination (namestring (merge-pathnames path (uiop:getcwd))))
         (revision (%revision manager root baseline))
         (id (loop for candidate = (format nil "~36R-~36R" (get-universal-time)
                                          (random (expt 2 80)))
                   unless (probe-file (%record-path manager candidate)) return candidate)))
    (when (probe-file destination)
      (error 'ownership-error :operation :create :detail "Destination already exists"))
    (when (eq dirty-policy :reject)
      (restart-case (%clean manager root)
        (use-committed-baseline ()
          :report "Use the committed baseline, excluding all dirty source changes."
          nil)))
    (%run manager root (list "worktree" "add" "--detach" "--" destination revision))
    (handler-case
        (let ((worktree (make-worktree :id id :owner owner :source root
                                      :path (%source manager destination) :baseline revision
                                      :git-directory (%git-directory manager destination))))
          (%write-record (%marker-path worktree) (%data worktree))
          (%write-record (%record-path manager id) (%data worktree))
          worktree)
      (error (condition)
        ;; Git's non-forced removal refuses any intervening uncommitted work.
        (handler-case
            (%run manager root (list "worktree" "remove" "--" destination))
          (error (cleanup-condition)
            (error 'cleanup-failure :operation :creation-rollback
                   :detail (format nil "~A; retained ~A: ~A"
                                   condition destination cleanup-condition))))
        (if (typep condition 'worktree-error)
            (error condition)
            (error 'worktree-error :operation :create :detail (princ-to-string condition)))))))

(defun discover-worktrees (manager &key owner source)
  "Return two values: owned handles and absent orphan handles.
With SOURCE, also discover Git markers whose registry publication was interrupted.
Discovery is read-only and never claims unmarked or foreign worktrees."
  (let ((trees (loop for file in (uiop:directory-files (manager-registry manager) "*.sexp")
                    collect (handler-case (apply #'make-worktree (%read-record file))
                              (error (condition)
                                (error 'ownership-error :operation :discovery
                                       :detail (princ-to-string condition)))))))
    (when source
      (let* ((root (%source manager source))
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

(defun cleanup-worktree (manager worktree &key owner discard-dirty)
  "Remove verified owned worktrees, returning T on removal or NIL on RETAIN-WORKTREE.
Explicit DISCARD-DIRTY permits Git --force. RETRY-CLEANUP revalidates ownership;
RETAIN-WORKTREE leaves recoverable failures registered without discarding edits."
  (restart-case
      (let ((status (%owned manager worktree owner :absent-ok t)))
        (handler-case
            (progn
              (unless (eq status :retired)
                (when (probe-file (worktree-path worktree))
                  (unless discard-dirty (%clean manager (worktree-path worktree))))
                (%run manager (worktree-source worktree)
                      (append '("worktree" "remove") (when discard-dirty '("--force"))
                              (list "--" (worktree-path worktree)))))
              (let ((record (%record-path manager (worktree-id worktree))))
                (when (probe-file record) (delete-file record)))
              (setf (worktree-state worktree) :removed)
              t)
          (dirty-workspace (condition) (error condition))
          (error (condition)
            (error 'cleanup-failure :operation :cleanup :detail (princ-to-string condition)))))
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
                          (:patch "patch") (:commit-range "commit-range"))
        :worktree-id (artifact-worktree-id artifact) :owner (artifact-owner artifact)
        :source (artifact-source artifact)
        :common-git-directory (artifact-common-git-directory artifact)
        :isolated-path (artifact-isolated-path artifact)
        :baseline (artifact-baseline artifact) :tip (artifact-tip artifact)
        :patch (artifact-patch artifact)
        :patch-reference (or patch-reference (artifact-patch-reference artifact))
        :commits (when (eq (artifact-kind artifact) :commit-range) (artifact-content artifact))
        :touched-paths (artifact-paths artifact)))

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
                         :source (getf data :source)
                         :common-git-directory (getf data :common-git-directory)
                         :isolated-path (getf data :isolated-path)
                         :patch (getf data :patch) :patch-reference (getf data :patch-reference)
                         :paths (getf data :touched-paths)
                         :content (if (eq kind :patch) (getf data :patch) (getf data :commits)))))
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
      (error 'dirty-workspace :operation :extract-patch
             :detail "Stage untracked files before patch extraction"))
    (when (integration-conflicts manager path)
      (error 'integration-conflict :operation :extract-patch :detail "Unmerged files"
             :paths (integration-conflicts manager path)))
    (let ((patch (%run manager path
                       (list "diff" "--no-ext-diff" "--no-textconv" "--binary" base "--"))))
      (%artifact manager worktree :kind :patch :tip (%tip manager worktree)
                 :content patch :patch patch
                 :paths (%split-zero (%run manager path
                                           (list "diff" "--no-renames" "--name-only" "-z" base "--")))))))

(defun %commits (manager path base tip)
  "Return the validated baseline-to-tip sequence, including an empty NIL range."
  (%run manager path (list "merge-base" "--is-ancestor" base tip))
  (remove "" (uiop:split-string
              (%trim (%run manager path (list "rev-list" "--topo-order" "--reverse"
                                             (format nil "~A..~A" base tip))))
              :separator '(#\Newline))
          :test #'equal))

(defun extract-commit-range (manager worktree &key owner)
  "Return ordered commits after BASELINE, requiring a clean descendant tip."
  (%owned manager worktree owner)
  (%clean manager (worktree-path worktree))
  (let* ((path (worktree-path worktree)) (base (worktree-baseline worktree))
         (tip (%tip manager worktree)))
    (%artifact manager worktree :kind :commit-range :tip tip
               :content (%commits manager path base tip)
               :patch (%run manager path
                            (list "diff" "--no-ext-diff" "--no-textconv" "--binary" base tip "--"))
               :paths (%split-zero (%run manager path
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
      (error 'integration-conflict :operation :busy-target
             :detail (format nil "Existing Git operation: ~A" name)
             :paths (integration-conflicts manager target))))
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
  (let* ((base (%revision manager target (artifact-baseline artifact)))
         (tip (%revision manager target (artifact-tip artifact)))
         (commits (%commits manager target base tip)))
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
