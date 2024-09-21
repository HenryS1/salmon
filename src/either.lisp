(defpackage :either
  (:use :cl :monad)
  (:import-from :monad :fmap :flatmap)
  (:export :right :left :value :err :if-absent))

(in-package :either)

(defstruct (left (:constructor left (err)) (:conc-name)) err)
(defstruct (right (:constructor right (value)) (:conc-name)) value)

(defun if-absent (v err) (or (and v (right v)) (left err)))

(defmethod print-object ((left left) out)
  (print-unreadable-object (left out :type t)
    (format out "~a" (err left))))

(defmethod print-object ((right right) out)
  (print-unreadable-object (right out :type t)
    (format out "~a" (value right))))

(defmethod fmap (fun (left left)) left)

(defmethod fmap (fun (right right))
  (right (funcall fun (value right))))

(defmethod flatmap (fun (left left)) left)

(defmethod flatmap (fun (right right))
  (funcall fun (value right)))
