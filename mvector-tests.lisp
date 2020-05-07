(defpackage :mvector-tests
  (:use :cl :rove :mvector))

(in-package :mvector-tests)

(defun vector-equal (one other)
  (every (lambda (a b) (equal a b)) one other))

(deftest fmap-test 
  (testing "fmap works the same as map 'vector"
    (let ((fun (lambda (x) (+ x 1))))
      (ok (vector-equal (monad:fmap fun (vector 1 2 3)) 
                        (mapcar fun '(1 2 3)))))))

(deftest flatmap-test
  (testing "flatmap returns the elements in order"
    (ok (vector (monad:flatmap (lambda (x) (vector 1 x)) (vector 2 3 4))
                (vector 1 2 1 3 1 4)))))
