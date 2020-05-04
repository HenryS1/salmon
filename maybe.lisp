(defpackage :maybe
  (:use :cl)
  (:export :value
           :just
           :nothing)) 

(in-package :maybe)

(defclass maybe () ())

(defclass nothing (maybe)
  ((value :accessor value :initform nil))) 

(defclass just (maybe)
  ((value :accessor value :initarg :value :initform (error "must initialize just with a value"))))

(defun just (v) (make-instance 'just :value v))

(defmethod print-object ((just just) out)
  (print-unreadable-object (just out :type t)
    (format out "~a" (value just))))
