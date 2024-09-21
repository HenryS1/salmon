(defpackage :maybe
  (:use :cl :monad)
  (:import-from :monad :fmap :flatmap)
  (:export :value
           :just))

(in-package :maybe)

(defstruct (just (:constructor just (value)) (:conc-name)) value)

(defmethod print-object ((just just) out)
  (print-unreadable-object (just out :type t)
    (format out "~a" (value just))))

(defmethod fmap (fun (v (eql nil))) v)

(defmethod fmap (fun (just just))
  (just (funcall fun (value just))))

(defmethod flatmap (fun (v (eql nil))) v)

(defmethod flatmap (fun (just just))
  (funcall fun (value just)))
