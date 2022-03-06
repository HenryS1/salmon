(defpackage :maybe-tests
  (:use :cl :rove :maybe))

(in-package :maybe-tests)

(deftest just-test 
    (testing "just-value is inverse of just"
      (ok (= (just-value (just 10)) 10))))

(deftest fmaptest
    (testing "fmap action on just and nothing"
      (ok (= (just-value (monad:fmap (lambda (x) (+ x 1)) (just 10)))
             11)
          "fmap applies a function to the value in just")
      (ok (eq (monad:fmap (lambda (x) (+ x 1)) nil) nil)
          "fmap leaves nil as nil")))

(deftest flatmap-test
    (testing "flatmap action on just and nothing"
      (ok (typep (just-value (monad:flatmap #'just (just 10))) 'integer)
          "flatmap does not nest just")
      (ok (= (just-value (monad:flatmap (lambda (x) (just (+ x 1))) (just 10))) 11)
          "flatmap applies a function to the value of just")
      (ok (eq (monad:flatmap (lambda (x) (+ x 1)) nil) nil)
          "flatmap leaves nil as nil")))
