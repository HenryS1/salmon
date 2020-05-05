(asdf:defsystem "hylo"
  :version "0.1.0"
  :author "Henry Steere"
  :license "MIT"
  :components ((:file "maybe")
               (:file "try")
               (:file "monad" :depends-on ("maybe" "try")))
  :description "Provides monad comprehensions in Common Lisp"
  :in-order-to ((test-op (test-op "lambdas-test"))))
