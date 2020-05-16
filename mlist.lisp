(defpackage :mlist
  (:use :cl :monad))

(in-package :mlist)

(defmethod fmap (fun (l list))
  (mapcar fun l))

(defmethod flatmap (fun (l list))
  (mapcan fun l))
