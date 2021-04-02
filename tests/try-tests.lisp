(defpackage :try-tests
  (:use :cl :rove :try))

(in-package :try-tests)

(define-condition a-test-condition (error) ())

(deftest try-test 
  (testing "try handles errors in its body"
    (let ((v (try (error 'a-test-condition))))
      (ok (typep v 'failure)
          "when a condition occurs a try returns failure")
      (ok (typep (value v) 'a-test-condition)
          "when a condition occurs the value in the failure is the condition"))
    (let ((v (try 10)))
      (ok (typep v 'success)
          "when no condition occurs a try returns success")
      (ok (= (value v) 10)
          "when no condition occurs the value of the expression is returned in a success"))))

(deftest fmap-test
    (testing "fmap action on success and failure"
      (ok (= (value (monad:fmap (lambda (x) (+ x 1)) (success 10))) 11)
          "fmap applies a function to the value in success")
      (let ((v (try (error 'a-test-condition))))
        (ok (eq (monad:fmap (lambda (x) (+ x 1)) v) v)
          "fmap leaves a failure unchanged"))))

(deftest flatmap-test
    (testing "flatmap action on success and failure"
      (ok (typep (value (monad:flatmap (lambda (x) (success (+ x 1))) (success 10))) 'integer)
          "flatmap does not nest successes")
      (ok (= (value (monad:flatmap (lambda (x) (success (+ x 1))) (success 10))) 11)
          "flatmap applies a function the value of a success")
      (let ((v (try (error 'a-test-condition))))
        (ok (eq (monad:flatmap (lambda (x) (+ x 1)) v) v)
            "flatmap leaves a failure unchanged"))))
