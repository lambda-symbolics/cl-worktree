(defpackage #:cl-worktree
  (:use #:cl)
  (:export #:make-manager #:manager #:worktree #:worktree-path #:worktree-source
           #:worktree-id #:worktree-owner #:worktree-baseline #:worktree-state
           #:create-worktree #:discover-worktrees #:cleanup-worktree
           #:artifact #:artifact-kind #:artifact-baseline #:artifact-tip
           #:artifact-content #:artifact-paths #:extract-patch #:extract-commit-range
           #:check-patch #:apply-patch #:apply-commit-range #:integration-conflicts
           #:abort-integration #:worktree-error #:failure-operation #:failure-detail
           #:command-failure #:failure-command #:failure-exit-code
           #:isolation-unavailable #:dirty-workspace #:ownership-error
           #:cleanup-failure #:integration-conflict #:conflict-paths #:run-tests))
