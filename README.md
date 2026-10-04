# cl-worktree

Owned, detached Git worktrees and explicit integration primitives for Common Lisp.
ISC licensed, copyright Lambda Symbolics. Requires Git and ASDF/UIOP. The library
has no process runner, task authority, sandbox, or integration-order policy.

## Runner and manager

Supply a function of `(argv &key directory input)` returning three values:
stdout string, stderr string, integer exit status. Arguments are an argv list,
never a shell command. Input is a string or NIL. The caller chooses execution
permissions, timeout, encoding, and process supervision. Nonzero status must be
returned, not raised by the runner, so the library can classify Git failures.

```lisp
(cl-worktree:make-manager
 :registry #P"/private/task-worktrees/"
 :runner (lambda (argv &key directory input)
           (uiop:run-program argv :directory directory
             :input (if input (make-string-input-stream input) :interactive)
             :output :string :error-output :string :ignore-error-status t)))
```

Keep the registry private and serialize operations on the same Git repository.
Git ref changes and clean-state checks are not atomic with external writers.

## Isolation lifecycle

`create-worktree` takes a manager and keyword arguments `source`, `path`,
`baseline`, `owner`, and `dirty-policy`. Baseline is a commit or commit-resolving
revision string. Paths are resolved relative to the process working directory.
The destination must not exist. A detached worktree avoids branch collisions.
The returned handle supplies `worktree-id`, `worktree-owner`, `worktree-path`,
`worktree-source`, `worktree-baseline`, and `worktree-state`.

Dirty policy defaults to `:reject`. Explicit `:ignore` uses only the chosen
committed baseline, excluding staged, unstaged, and untracked source edits.
Creation never checks out, resets, or stashes the source. A non-Git source
signals `isolation-unavailable`.

`discover-worktrees` returns persisted handles, optionally filtered by `owner`.
Compare their paths with the filesystem to identify absent orphan worktrees.
Registry records survive failed cleanup and process restart.

`cleanup-worktree` requires the owner and verifies the private registry and Git
directory identity marker. Dirty worktrees raise `dirty-workspace`; explicit
`discard-dirty` enables forced removal. Only this worktree is removed. Branches
and other worktrees are not deleted or pruned. Failed removal raises
`cleanup-failure` and retains the registry for retry.

Ownership protects against accidental deletion, not hostile modifications by
another process with access to the registry and Git metadata.

## Artifacts and integration

`extract-patch` requires the owner and returns an artifact including committed,
staged and unstaged tracked changes relative to the recorded baseline. Stage
new files first. Git binary patches preserve binary content and file modes.
`extract-commit-range` requires a clean worktree and an ancestor baseline;
its content is an ordered list of commits. Both include kind, baseline, tip,
and touched paths through `artifact-*` accessors.

`check-patch` checks applicability without mutation. `apply-patch` checks and
applies a patch to the clean target's index and working tree. Supply the patch
string, typically `artifact-content`. `apply-commit-range` cherry-picks a range
artifact into a clean target; commits must be available in its object database.
Integration targets are explicitly chosen by the caller, not inferred from an
artifact's source.

An `integration-conflict` supplies `conflict-paths` and `failure-detail`.
`integration-conflicts` returns the current unmerged paths. A failed cherry-pick
leaves Git's conflict state for inspection or resolution; `abort-integration`
explicitly restores Git's pre-cherry-pick state. Patch check failures leave the
target untouched. The caller controls acceptance, ordering and commits.

All domain conditions derive from `worktree-error`, with `failure-operation`
and `failure-detail`. `command-failure` additionally supplies the command and
exit code. Ownership and dirty-state refusals have their own condition types.

## Tests

From this repository:

```sh
sbcl --noinform --non-interactive --eval '(require :asdf)' \
  --eval '(asdf:load-asd (truename "cl-worktree.asd"))' \
  --eval '(asdf:test-system "cl-worktree")'
```

Tests create temporary real Git repositories and exercise source preservation,
dirty policy, ownership, artifact extraction, clean application, conflict abort,
and cleanup failures. Temporary repositories are removed after each run.
