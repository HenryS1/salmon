(defpackage :maybe
  (:use :cl :monad)
  (:import-from :monad :fmap :flatmap)
  (:export :value
           :just
           :nothing))

(in-package :maybe)

(defclass maybe () ())

(defclass nothing (maybe)
  ((value :accessor value :initform nil))) 

(defclass just (maybe)
  ((value :accessor value :initarg :value :initform (error "must initialize just with a value"))))

(defvar *nothing* (make-instance 'nothing))

(defun nothing () *nothing*)
(defun just (v) (make-instance 'just :value v))

(defmethod print-object ((just just) out)
  (print-unreadable-object (just out :type t)
    (format out "~a" (value just))))

(defmethod fmap (fun (nothing nothing)) nothing)

(defmethod fmap (fun (just just))
  (just (funcall fun (value just))))

(defmethod flatmap (fun (nothing nothing)) nothing)

(defmethod flatmap (fun (just just))
  (funcall fun (value just)))
