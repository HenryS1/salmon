(asdf:defsystem "hylo"
  :version "0.1.0"
  :author "Henry Steere"
  :license "MIT"
  :components ((:file "maybe")
               (:file "monad" :depends-on ("maybe")))
  :description "Provides abbreviated lambdas for Common Lisp"
  :in-order-to ((test-op (test-op "lambdas-test"))))
