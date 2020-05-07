(defpackage :mvector
  (:use :cl :monad))

(in-package :mvector)

(defmethod fmap (fun (v vector))
  (map 'vector fun v))

(defmethod flatmap (fun (v vector))
  (let* ((vs (fmap fun v))
         (result (make-array (reduce (lambda (a b) (+ a (length b))) vs :initial-value 0))))
    (loop with i = 0
       for v across vs
       do (loop for el across v
             do (setf (aref result i) el)
               (incf i)))
    result))
