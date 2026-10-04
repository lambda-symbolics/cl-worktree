(asdf:defsystem "cl-worktree"
  :description "Owned Git worktree isolation and integration primitives"
  :author "Lambda Symbolics"
  :license "ISC"
  :version "0.1.0"
  :depends-on ("uiop")
  :serial t
  :components ((:file "package") (:file "worktree"))
  :in-order-to ((asdf:test-op (asdf:test-op "cl-worktree/tests"))))

(asdf:defsystem "cl-worktree/tests"
  :depends-on ("cl-worktree")
  :serial t
  :components ((:file "tests"))
  :perform (asdf:test-op (operation system)
             (declare (ignore operation system))
             (uiop:symbol-call :cl-worktree :run-tests)))
