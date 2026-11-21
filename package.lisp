(defpackage #:cl-worktree
  (:use #:cl)
  (:documentation "A library for managing owned, detached Git worktrees, with the ability to extract the patches and commit ranges from them.")
  (:export #:make-manager #:manager #:worktree #:worktree-path #:worktree-source
           #:worktree-id #:worktree-owner #:worktree-baseline #:worktree-state
           #:describe-repository #:repository-primary-checkout #:repository-origin
             #:checkout-admin-roots #:git-read-argv
           #:create-worktree #:recover-worktree #:discover-worktrees #:cleanup-worktree
             #:recover-worktree-by-identity
           #:use-committed-baseline #:retain-worktree #:retry-cleanup
           #:artifact #:artifact-kind #:artifact-baseline #:artifact-tip
           #:artifact-content #:artifact-paths #:extract-patch #:extract-commit-range
           #:artifact->plist #:plist->artifact #:artifact-owner #:artifact-source
           #:artifact-common-git-directory #:artifact-isolated-path #:artifact-worktree-id
           #:artifact-patch #:artifact-patch-reference
           #:check-patch #:apply-patch #:apply-commit-range #:integration-conflicts
           #:abort-integration #:worktree-error #:failure-operation #:failure-detail
           #:command-failure #:failure-command #:failure-exit-code
           #:isolation-unavailable #:dirty-workspace #:ownership-error
             #:worktree-identity-mismatch
           #:cleanup-failure #:integration-conflict #:conflict-paths #:run-tests))
