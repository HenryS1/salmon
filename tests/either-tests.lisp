(defpackage :either-tests
  (:use :cl :rove :either))

(in-package :either-tests)

(deftest fmap-test
  (testing "fmap leaves left unchanged"
    (let ((fun (lambda (x) (+ x 1))))
      (ok (equalp (monad:fmap fun (left "an error")) (left "an error")))))
  (testing "fmap applies a function to the value in a right"
    (let ((fun (lambda (x) (+ x 1))))
      (ok (equalp (monad:fmap fun (right 10)) (right 11))))))

(deftest flatmap-test
  (testing "flatmap leaves left unchanged"
    (let ((fun (lambda (x) (right (+ x 1)))))
      (ok (equalp (monad:flatmap fun (left "an error")) (left "an error")))))
  (testing "flatmap flattens the result of applying a function to the value in a right"
    (let ((fun (lambda (x) (right (+ x 1)))))
      (ok (equalp (monad:flatmap fun (right 10)) (right 11))))
    (let ((fun (lambda (x) (declare (ignore x)) (left "an error"))))
      (ok (equalp (monad:flatmap fun (right 10)) (left "an error"))))))

(deftest if-absent
  (let ((table (make-hash-table :test 'equal)))
    (setf (gethash "hello" table) "there")
    (testing "returns error value when option is nil"
      (ok (equalp (if-absent (gethash "absent" table) "not found")
                  (left "not found"))))
    (testing "returns value when option is not nil"
      (ok (equalp (if-absent (gethash "hello" table) "not found")
                  (right "there"))))))
