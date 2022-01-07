(defpackage :maybe
  (:use :cl :monad)
  (:import-from :monad :fmap :flatmap)
  (:export :just-value
           :just))

(in-package :maybe)

(defstruct just value)

(defun just (v) (make-just :value v))

(defmethod print-object ((just just) out)
  (print-unreadable-object (just out :type t)
    (format out "~a" (just-value just))))

(defmethod fmap (fun (v (eql nil))) v)

(defmethod fmap (fun (just just))
  (just (funcall fun (just-value just))))

(defmethod flatmap (fun (v (eql nil))) v)

(defmethod flatmap (fun (just just))
  (funcall fun (just-value just)))
