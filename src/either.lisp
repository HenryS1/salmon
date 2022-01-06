(defpackage :either
  (:use :cl :monad)
  (:import-from :monad :fmap :flatmap)
  (:export :value :right :left))

(in-package :either)

(defstruct left err)
(defstruct right value)

(defun left (e) (make-left :err e))
(defun right (v) (make-right :value v))

(defmethod print-object ((left left) out)
  (print-unreadable-object (left out :type t)
    (format out "~a" (left-err left))))

(defmethod print-object ((right right) out)
  (print-unreadable-object (right out :type t)
    (format out "~a" (right-value right))))

(defmethod fmap (fun (left left)) left)

(defmethod fmap (fun (right right))
  (right (funcall fun (right-value right))))

(defmethod flatmap (fun (left left)) left)

(defmethod flatmap (fun (right right))
  (funcall fun (right-value right)))
