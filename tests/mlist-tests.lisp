(defpackage :mlist-tests
  (:use :cl :rove :mlist))

(in-package :mlist-tests)

(deftest fmap-test 
  (testing "fmap works the same as mapcar"
    (let ((fun (lambda (x) (+ x 1))))
      (ok (equal (monad:fmap fun '(1 2 3)) 
                 (mapcar fun '(1 2 3)))))))

(deftest flatmap-test
  (testing "flatmap returns the elements in order"
    (ok (equal (monad:flatmap (lambda (x) (list 1 x)) '(2 3 4))
               '(1 2 1 3 1 4)))))
