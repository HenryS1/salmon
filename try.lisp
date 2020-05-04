(defpackage :try
  (:use :cl)
  (:export :success
           :try
           :failure
           :value))

(in-package :try)

(defclass try () ())

(defclass success (try) 
  ((value :accessor value :initarg :value :initform (error "value must be provided"))))

(defclass failure (try)
  ((value :accessor value :initarg :value :initform (error "value must be provided"))))

(defun success (v)
  (make-instance 'success :value v))

(defun failure (v)
  (make-instance 'failure :value v))

(defmacro try (&rest exps)
  `(handler-case (success (progn ,@exps))
     (t (e) () (failure e))))

(defmethod print-object ((success success) out)
  (print-unreadable-object (success out :type t)
    (format out "~a" (value success))))

(defmethod print-object ((failure failure) out)
  (print-unreadable-object (failure out :type t)
    (format out "~a" (value failure))))
