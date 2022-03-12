(defpackage :monad-tests
  (:use :cl :rove :monad :either))

(in-package :monad-tests)

(deftest test-mdo
  (testing "mdo processes maybe values"
    (ok (eq (mdo (a (maybe:just 10))
                    (b (maybe:just 11))
                    (c nil)
                    (yield (+ a b c)))
               nil)
        "mdo short circuits when a nil value occurs")
    (ok (typep (mdo (a (maybe:just 6))
                    (b (maybe:just 4))
                    (yield (+ a b)))
               'maybe:just)
        "mdo returns just when no nils occur in a comprehension")
    (ok (equalp (mdo (a (maybe:just 6))
                     (b (maybe:just 4))
                     (yield (+ a b)))
                (maybe:just 10))
        "mdo unwraps values from just")
    (ok (equalp (mdo (a (maybe:just 7))
                     (let (b 2))
                     (yield (+ a b)))
                (maybe:just 9))
        "mdo binds one value in a let clause")
    (ok (equalp (mdo (a (maybe:just 7))
                     (let (b 2) (c 1))
                     (yield (+ a b c)))
                (maybe:just 10))
        "mdo binds multiple values in a let clause")
    (ok (equalp (mdo (a (maybe:just 7))
                     (let (b 3))
                     (c (maybe:just 2))
                     (let (d 1))
                     (e (maybe:just 9))
                     (yield (+ a b c d e)))
                (maybe:just 22))
        "mdo binds values in multiple let clauses")))

(deftest error-handling
  (testing "mdo handles errors in a yield clause"
    (ok (equalp (mdo (handle division-by-zero () (left "Error occurred"))
                     (a (right 0))
                     (yield (/ 10 a))) 
                (left "Error occurred"))))
  (testing "mdo handles errors in a body clause"
    (ok (equalp (mdo (handle division-by-zero () (left "Error occurred"))
                     (a (right (/ 1 0)))
                     (b (right 2))
                     (yield (+ a b)))
                (left "Error occurred")))))

(deftest resource-management
  (testing "with clause calls tidy up function after executing"
    (let ((f (open "test.tst" :direction :output :if-exists :error)))
      (ok (probe-file "test.tst"))
      (ok (equalp 
           (mdo (with (lambda (f) (progn (close f) (delete-file "test.tst"))) writer (right f))
                (_ (format writer "hello~%") (right (finish-output writer)))
                (handle error (e) (left (format nil "Error reading file ~a" e)))
                (with #'close reader (right (open "test.tst" :direction :input)))
                (b (right (read-line reader nil nil)))
                (yield b))
           (right "hello")))
      (ok (not (probe-file "test.tst"))))))
