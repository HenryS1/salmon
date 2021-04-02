(defsystem "salmon"
  :version "1.0.0"
  :author "Henry Steere"
  :license "MIT"
  :components ((:module "src"
                :components 
                ((:file "monad")
                 (:file "maybe" :depends-on ("monad"))
                 (:file "try" :depends-on ("monad"))
                 (:file "mlist" :depends-on ("monad"))
                 (:file "mvector" :depends-on ("monad")))))
  :description "Provides monad comprehensions in Common Lisp"
  :in-order-to ((test-op (test-op "salmon-tests"))))

(defsystem "salmon/tests"
  :depends-on ("rove"
               "salmon")
  :components ((:module "tests"
                :components
                ((:file "maybe-tests")
                 (:file "try-tests")
                 (:file "monad-tests")
                 (:file "mlist-tests")
                 (:file "mvector-tests"))))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
