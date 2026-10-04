(in-package #:cl-worktree)

(defvar *checks* 0)

(defun test-runner (argv &key directory input)
  "Execute real Git with a string input stream and capture all three results."
  (uiop:run-program argv :directory directory
                    :input (if input (make-string-input-stream input) :interactive)
                    :output :string :error-output :string :ignore-error-status t))

(defun verify (value)
  "Record an assertion."
  (incf *checks*)
  (unless value (error "Failed worktree check ~D" *checks*)))

(defmacro fails (type &body body)
  "Assert the expected typed failure."
  `(verify (handler-case (progn ,@body nil) (,type () t))))

(defun write-text (path text)
  "Write fixture contents."
  (with-open-file (stream path :direction :output :if-exists :supersede)
    (write-string text stream)))

(defun commit-text (manager path text &optional (message "change"))
  "Commit a fixture edit."
  (write-text (merge-pathnames "file" (uiop:ensure-directory-pathname path)) text)
  (%run manager path '("add" "file"))
  (%run manager path (list "commit" "-m" message)))

(defun run-tests ()
  "Exercise real temporary Git repositories, failures and durable ownership."
  (let* ((*checks* 0)
         (root (merge-pathnames (format nil "cl-worktree-tests-~36R/" (random (expt 2 80)))
                               (uiop:temporary-directory)))
         (source (merge-pathnames "source/" root))
         (registry (merge-pathnames "registry/" root))
         (manager (make-manager :runner #'test-runner :registry registry)))
    (unwind-protect
         (progn
           (ensure-directories-exist (merge-pathnames "probe" source))
           (fails isolation-unavailable
             (create-worktree manager :source source :path (merge-pathnames "bad" root)
                              :baseline "HEAD" :owner "task"))
           (%run manager source '("init"))
           (%run manager source '("config" "user.name" "Worktree Tests"))
           (%run manager source '("config" "user.email" "tests@example.invalid"))
           (commit-text manager source (format nil "base~%") "baseline")
           (let* ((base (%revision manager source "HEAD"))
                  (branch (%trim (%run manager source '("symbolic-ref" "HEAD"))))
                  (tree (create-worktree manager :source source
                                         :path (merge-pathnames "child" root)
                                         :baseline base :owner "task")))
             (verify (equal base (worktree-baseline tree)))
             (verify (= 1 (length (discover-worktrees manager :owner "task"))))
             (verify (null (discover-worktrees manager :owner "other")))
             (fails ownership-error (cleanup-worktree manager tree :owner "other"))
             (verify (probe-file (worktree-path tree)))
             (fails ownership-error
               (create-worktree manager :source source :path (worktree-path tree)
                                :baseline base :owner "task"))
             (write-text (merge-pathnames "untracked" source) "dirt")
             (fails dirty-workspace
               (create-worktree manager :source source :path (merge-pathnames "dirty" root)
                                :baseline base :owner "task"))
             (let ((ignored (create-worktree manager :source source
                                             :path (merge-pathnames "ignored" root)
                                             :baseline base :owner "task"
                                             :dirty-policy :ignore)))
               (verify (not (probe-file (merge-pathnames "untracked"
                                                       (uiop:ensure-directory-pathname
                                                        (worktree-path ignored))))))
               (verify (equal base (%revision manager source "HEAD")))
               (cleanup-worktree manager ignored :owner "task"))
             (verify (probe-file (merge-pathnames "untracked" source)))
             (fails dirty-workspace (apply-patch manager source "invalid"))
             (delete-file (merge-pathnames "untracked" source))
             (let ((path (uiop:ensure-directory-pathname (worktree-path tree))))
               (write-text (merge-pathnames "file" path) (format nil "child~%"))
               (fails dirty-workspace (cleanup-worktree manager tree :owner "task"))
               (let ((patch (extract-patch manager tree :owner "task")))
                 (verify (equal '("file") (artifact-paths patch)))
                 (verify (check-patch manager source (artifact-content patch)))
                 (verify (apply-patch manager source (artifact-content patch)))
                 (verify (search "child" (%run manager source '("diff" "--cached"))))
                 (%run manager source '("reset" "--hard" "HEAD")))
               (commit-text manager path (format nil "child~%"))
               (let ((range (extract-commit-range manager tree :owner "task")))
                 (verify (= 1 (length (artifact-content range))))
                 (verify (apply-commit-range manager source range))
                 (verify (equal (artifact-tip range) (%revision manager source "HEAD")))
                 (%run manager source (list "reset" "--hard" base))
                 (commit-text manager source (format nil "parent~%"))
                 (let ((parent-tip (%revision manager source "HEAD")))
                   (fails integration-conflict (apply-commit-range manager source range))
                   (verify (equal '("file") (integration-conflicts manager source)))
                   (verify (abort-integration manager source))
                   (verify (equal parent-tip (%revision manager source "HEAD")))
                   (verify (null (integration-conflicts manager source)))
                   (fails integration-conflict
                     (check-patch manager source (artifact-content
                                                  (extract-patch manager tree :owner "task")))))
                 (verify (equal branch (%trim (%run manager source '("symbolic-ref" "HEAD"))))))
               ;; Cleanup errors preserve the durable handle for retry.
               (let ((failing (make-manager
                               :registry registry
                               :runner (lambda (argv &key directory input)
                                         (if (equal (subseq argv 1 (min 3 (length argv)))
                                                    '("worktree" "remove"))
                                             (values "" "injected removal failure" 1)
                                             (test-runner argv :directory directory :input input))))))
                 (fails cleanup-failure (cleanup-worktree failing tree :owner "task"))
                 (verify (= 1 (length (discover-worktrees manager)))))
               ;; A replacement marker must never authorize deletion.
               (let ((marker (%marker-path tree)) (data (%data tree)))
                 (%write-record marker '(:id "other"))
                 (fails ownership-error (cleanup-worktree manager tree :owner "task"))
                 (verify (probe-file path))
                 (%write-record marker data))
               (write-text (merge-pathnames "scratch" path) "discard")
               (verify (cleanup-worktree manager tree :owner "task" :discard-dirty t))
               (verify (eq :removed (worktree-state tree)))
               (verify (null (discover-worktrees manager)))
               (verify (not (probe-file path)))))
           (format t "~D cl-worktree checks passed.~%" *checks*)
           t)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))
