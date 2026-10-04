(in-package #:cl-worktree)

(define-condition worktree-error (error)
  ((operation :initarg :operation :reader failure-operation)
   (detail :initarg :detail :reader failure-detail))
  (:report (lambda (condition stream)
             (format stream "~A: ~A" (failure-operation condition)
                     (failure-detail condition))))
  (:documentation "A typed lifecycle or integration failure."))

(define-condition command-failure (worktree-error)
  ((command :initarg :command :reader failure-command)
   (exit-code :initarg :exit-code :reader failure-exit-code))
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
  ((paths :initarg :paths :reader conflict-paths))
  (:documentation "Integration failed, possibly leaving explicit conflict state."))

(defstruct (manager (:constructor %make-manager))
  "Command runner and a private durable registry directory."
  runner registry)
(defstruct worktree
  "A detached worktree identity; registry records persist across process restarts."
  id owner source path baseline git-directory (state :active))
(defstruct artifact
  "A baseline-relative change artifact with touched paths."
  kind baseline tip content paths)

(defun make-manager (&key runner registry)
  "RUNNER accepts argv and :DIRECTORY/:INPUT, returning stdout, stderr, exit code.
REGISTRY must be a private directory. Callers serialize operations per repository."
  (check-type runner function)
  (let ((path (uiop:ensure-directory-pathname
               (merge-pathnames registry (uiop:getcwd)))))
    (ensure-directories-exist (merge-pathnames "probe" path))
    (%make-manager :runner runner :registry path)))

(defun %run (manager directory arguments &key input (check t))
  "Invoke Git without a shell; optionally signal a command failure."
  (multiple-value-bind (out err code)
      (funcall (manager-runner manager) (cons "git" arguments)
               :directory directory :input input)
    (unless (and (stringp out) (stringp err) (integerp code))
      (error 'worktree-error :operation :runner :detail "Invalid runner result"))
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
  "Resolve the source root or report unavailability."
  (multiple-value-bind (out err code)
      (%run manager source '("rev-parse" "--show-toplevel") :check nil)
    (unless (zerop code)
      (error 'isolation-unavailable :operation :source :detail err))
    (%trim out)))

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
  "Read metadata with reader evaluation disabled."
  (with-open-file (stream path)
    (with-standard-io-syntax
      (let ((*read-eval* nil)) (read stream)))))

(defun %owned (manager worktree owner &key absent-ok)
  "Verify registry, requested owner and private marker before mutation."
  (unless (and (equal owner (worktree-owner worktree))
               (equal (%data worktree)
                      (%read-record (%record-path manager (worktree-id worktree)))))
    (error 'ownership-error :operation :ownership :detail "Registry or owner mismatch"))
  (cond
    ((probe-file (worktree-path worktree))
     (unless (and (equal (%git-directory manager (worktree-path worktree))
                         (worktree-git-directory worktree))
                  (equal (%data worktree) (%read-record (%marker-path worktree))))
       (error 'ownership-error :operation :ownership :detail "Git marker mismatch")))
    ((not absent-ok)
     (error 'ownership-error :operation :ownership :detail "Worktree is absent")))
  t)

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
    (when (eq dirty-policy :reject) (%clean manager root))
    (%run manager root (list "worktree" "add" "--detach" "--" destination revision))
    (let ((worktree (make-worktree :id id :owner owner :source root :path destination
                                  :baseline revision
                                  :git-directory (%git-directory manager destination))))
      (handler-case
          (progn (%write-record (%marker-path worktree) (%data worktree))
                 (%write-record (%record-path manager id) (%data worktree)))
        (error (condition)
          ;; Removal cannot discard child work: the worktree was just created.
          (%run manager root (list "worktree" "remove" "--" destination) :check nil)
          (error condition)))
      worktree)))

(defun discover-worktrees (manager &key owner)
  "Return durable handles, including absent worktrees (orphans).
Records are not removed merely because a worktree disappeared. Inspect STATE and PATH."
  (loop for file in (uiop:directory-files (manager-registry manager) "*.sexp")
        for data = (%read-record file)
        for tree = (apply #'make-worktree data)
        when (or (null owner) (equal owner (worktree-owner tree))) collect tree))

(defun cleanup-worktree (manager worktree &key owner discard-dirty)
  "Remove only a verified owned worktree. Explicit DISCARD-DIRTY permits Git --force.
A failed removal retains its registry. Absent paths with existing Git metadata are
reported, not pruned globally; only a successful Git removal deletes the registry."
  (%owned manager worktree owner :absent-ok t)
  (handler-case
      (progn
        (when (probe-file (worktree-path worktree))
          (unless discard-dirty (%clean manager (worktree-path worktree))))
        (%run manager (worktree-source worktree)
              (append '("worktree" "remove") (when discard-dirty '("--force"))
                      (list "--" (worktree-path worktree))))
        (delete-file (%record-path manager (worktree-id worktree)))
        (setf (worktree-state worktree) :removed)
        t)
    (dirty-workspace (condition) (error condition))
    (error (condition)
      (error 'cleanup-failure :operation :cleanup :detail (princ-to-string condition)))))

(defun %split-zero (text)
  "Split Git's NUL-delimited names, preserving spaces and newlines."
  (remove "" (uiop:split-string text :separator '(#\Null)) :test #'equal))

(defun %tip (manager worktree)
  "Resolve the isolated current commit."
  (%revision manager (worktree-path worktree) "HEAD"))

(defun extract-patch (manager worktree &key owner)
  "Return a binary-capable patch of committed, staged and unstaged tracked changes.
Untracked files require explicit staging in the isolated worktree before extraction."
  (%owned manager worktree owner)
  (let ((path (worktree-path worktree)) (base (worktree-baseline worktree)))
    (make-artifact :kind :patch :baseline base :tip (%tip manager worktree)
                   :content (%run manager path (list "diff" "--binary" base "--"))
                   :paths (%split-zero (%run manager path
                                             (list "diff" "--name-only" "-z" base "--"))))))

(defun extract-commit-range (manager worktree &key owner)
  "Return ordered commits after BASELINE, requiring a clean descendant tip."
  (%owned manager worktree owner)
  (%clean manager (worktree-path worktree))
  (let* ((path (worktree-path worktree)) (base (worktree-baseline worktree))
         (tip (%tip manager worktree)))
    (%run manager path (list "merge-base" "--is-ancestor" base tip))
    (make-artifact :kind :commit-range :baseline base :tip tip
                   :content (uiop:split-string
                             (%trim (%run manager path (list "rev-list" "--reverse"
                                                            (format nil "~A..~A" base tip))))
                             :separator '(#\Newline))
                   :paths (%split-zero (%run manager path
                                             (list "diff" "--name-only" "-z" base tip "--"))))))

(defun integration-conflicts (manager target)
  "Return unmerged target paths."
  (%split-zero (%run manager target '("diff" "--name-only" "--diff-filter=U" "-z"))))

(defun check-patch (manager target patch)
  "Check PATCH applicability without mutation; return T or signal a conflict."
  (%clean manager target)
  (multiple-value-bind (out err code)
      (%run manager target '("apply" "--check" "--index" "-") :input patch :check nil)
    (declare (ignore out))
    (unless (zerop code)
      (error 'integration-conflict :operation :check-patch :detail err :paths nil))
    t))

(defun apply-patch (manager target patch)
  "Apply PATCH to a clean target index and working tree after checking it."
  (check-patch manager target patch)
  (multiple-value-bind (out err code)
      (%run manager target '("apply" "--index" "-") :input patch :check nil)
    (declare (ignore out))
    (unless (zerop code)
      (error 'integration-conflict :operation :apply-patch :detail err
             :paths (integration-conflicts manager target)))
    t))

(defun apply-commit-range (manager target artifact)
  "Cherry-pick ARTIFACT commits into a clean target. Conflicts are left for inspection.
Commits must be present in the target object database. Caller controls integration."
  (%clean manager target)
  (unless (eq (artifact-kind artifact) :commit-range)
    (error 'worktree-error :operation :apply-range :detail "Expected commit-range artifact"))
  (let ((commits (remove "" (artifact-content artifact) :test #'equal)))
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
