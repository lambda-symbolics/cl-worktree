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

Use a runner with a controlled environment: clear inherited repository selectors
such as `GIT_DIR`, `GIT_WORK_TREE`, `GIT_INDEX_FILE`, `GIT_COMMON_DIR`,
`GIT_OBJECT_DIRECTORY`, `GIT_ALTERNATE_OBJECT_DIRECTORIES`, `GIT_NAMESPACE`,
`GIT_CONFIG_PARAMETERS`, and `GIT_CONFIG_COUNT`. Choose the executable and trusted
PATH in the runner. The sample above assumes an already controlled environment.

Every invocation uses per-command options to disable hooks, fsmonitor, optional
index refresh locks, automatic maintenance, signing, rerere and submodule recursion.
Configured external clean/smudge/process filters are discovered by key name and
disabled without reading their values into results. Diffs disable external diff
and textconv. These overrides do not edit global or repository configuration.
Worktree checkout and comparison use raw repository contents rather than external
filter transformations. Permission and timeout decisions belong to the runner.

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

`discover-worktrees` returns two values: owned handles and absent orphan handles,
optionally filtered by `owner`. With `source`, it also scans private Git markers,
recovering handles if registry publication was interrupted. Discovery is read-only
and excludes unmarked worktrees. Registry records survive failed cleanup and
process restart. Externally pruned orphans can retire their own registry record
without pruning or removing unrelated metadata.

`cleanup-worktree` requires the owner and verifies the private registry and Git
directory identity marker. Dirty worktrees raise `dirty-workspace`; explicit
`discard-dirty` enables forced removal. Only this worktree is removed. Branches
and other worktrees are not deleted or pruned. Failed removal raises
`cleanup-failure` and retains the registry for retry.

Recoverable lifecycle failures expose explicit restarts. During dirty-source
creation, `use-committed-baseline` selects the same behavior as `:ignore`.
During cleanup, `retain-worktree` returns NIL without continuing removal;
`retry-cleanup` retries after repair and revalidates ownership. Successful cleanup
returns T. Restarts never authorize discarding child edits implicitly.

Ownership protects against accidental deletion, not hostile modifications by
another process with access to the registry and Git metadata.

## Artifacts and integration

`extract-patch` requires the owner and returns an artifact including committed,
staged and unstaged tracked changes relative to the recorded baseline. Untracked
files are rejected; stage new files first. Git binary patches preserve binary
content and file modes. `extract-commit-range` requires a clean worktree and an
ancestor baseline; its content is an ordered list of commits. Both include a binary
patch and complete provenance through `artifact-*` accessors.

`artifact->plist` produces portable version-one data. `plist->artifact` validates
and restores it. The fields are:

- `:version` (1), `:kind` (`"patch"` or `"commit-range"`);
- `:worktree-id`, `:owner`, `:source`, `:common-git-directory`, `:isolated-path`;
- `:baseline` and `:tip` (full commit identities);
- `:patch` (complete binary-capable patch content), optional `:patch-reference`;
- `:commits` (ordered identities for a commit range), `:touched-paths`.

The plist contains only portable scalar values and lists. Convert keyword keys
and lists using the caller's JSON serializer. Supply `patch-reference` to
`artifact->plist` to identify a separately secured patch. Content is retained.

Extraction never cleans up the worktree. Secure the portable artifact durably,
then explicitly call `cleanup-worktree` when task policy permits removal. A range
artifact includes a patch suitable for later integration even if its original
commit objects become unavailable after Git garbage collection.

`check-patch` checks applicability without mutation. `apply-patch` checks and
applies a patch to the clean target's index and working tree. Supply the patch
string, typically `artifact-content`. `apply-commit-range` cherry-picks a range
artifact into a clean target; commits must be available in its object database.
Integration targets are explicitly chosen by the caller, not inferred from an
artifact's source.
Integration also refuses targets with a pending merge, rebase, revert or cherry-pick
operation, even when their index and files are clean. The refusal preserves that
operation for its owner.

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
dirty indexes and untracked files, ownership and orphan recovery, binary patches,
portable secured artifacts, range validation, conflict abort and cleanup failures.
On Unix they exercise suppression of executable hooks, fsmonitor and external
filters. Temporary repositories are removed after each run.
