(defpackage :maybe-tests
  (:use :cl :rove :maybe))

(in-package :maybe-tests)

(deftest just-test 
    (testing "value is inverse of just"
      (ok (= (value (just 10)) 10))))

(deftest fmaptest
    (testing "fmap action on just and nothing"
      (ok (= (value (monad:fmap (lambda (x) (+ x 1)) (just 10)))
             11)
          "fmap applies a function to the value in just")
      (ok (typep (monad:fmap (lambda (x) (+ x 1)) (nothing)) 'nothing)
          "fmap leaves nothing as nothing")))

(deftest flatmap-test
    (testing "flatmap action on just and nothing"
      (ok (typep (value (monad:flatmap #'just (just 10))) 'integer)
          "flatmap does not nest just")
      (ok (= (value (monad:flatmap (lambda (x) (just (+ x 1))) (just 10))) 11)
          "flatmap applies a function to the value of just")
      (ok (typep (monad:flatmap (lambda (x) (+ x 1)) (nothing)) 'nothing)
          "flatmap leaves nothing as nothing")))

(deftest nothing-instance-test
    (testing "constructing nothing always returns the same instance"
      (ok (eq (nothing) (nothing)))))
