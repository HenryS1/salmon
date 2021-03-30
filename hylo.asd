(defsystem "hylo"
  :version "0.1.0"
  :author "Henry Steere"
  :license "MIT"
  :components ((:file "monad")
               (:file "range" :depends-on ("monad"))
               (:file "maybe" :depends-on ("monad"))
               (:file "try" :depends-on ("monad"))
               (:file "mlist" :depends-on ("monad"))
               (:file "mvector" :depends-on ("monad")))
  :description "Provides monad comprehensions in Common Lisp"
  :in-order-to ((test-op (test-op "hylo-tests"))))

(defsystem "hylo-tests"
  :depends-on ("rove"
               "hylo")
  :components ((:file "maybe-tests")
               (:file "try-tests")
               (:file "monad-tests")
               (:file "mlist-tests")
               (:file "mvector-tests"))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
