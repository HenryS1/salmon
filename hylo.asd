(asdf:defsystem "hylo"
  :version "0.1.0"
  :author "Henry Steere"
  :license "MIT"
  :components ((:file "monad")
               (:file "maybe" :depends-on ("monad"))
               (:file "try" :depends-on ("monad"))
               (:file "mlist" :depends-on ("monad"))
               (:file "mvector" :depends-on ("monad")))
  :description "Provides monad comprehensions in Common Lisp"
  :in-order-to ((asdf:test-op (asdf:test-op "hylo-tests"))))

(asdf:defsystem "hylo-tests"
  :depends-on ("rove"
               "hylo")
  :components ((:file "maybe-tests")
               (:file "try-test")
               (:file "mlist-tests")
               (:file "mvector-tests"))
  :perform (asdf:test-op (o c) (uiop:symbol-call :rove '#:run c)))
