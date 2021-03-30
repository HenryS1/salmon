(defpackage :range
  (:use :cl :monad)
  (:export :between :to-array :append-ranges :reduce-range :index))

(in-package :range)

(defclass range ()
  ((size :accessor size :initarg :size)
   (transformation :accessor transformation :initarg :transformation :initform #'identity)))

(defun between (start end)
  (make-instance 'range :size (+ (- end start) 1) 
                 :transformation (lambda (i) (when (<= i (- end start)) (+ i start)))))

(defun to-array (range &key (type t))
  (let ((arr (make-array (size range) :element-type type :adjustable nil))
        (f (transformation range)))
    (loop for i from 0 to (- (size range) 1)
       do (setf (aref arr i) (funcall f i)))
    arr))

(defun append-ranges (one other)
  (let* ((one-f (transformation one))
         (other-f (transformation other))
         (new-f (lambda (i) 
                  (if (>= i (size one))
                      (funcall other-f (- i (size one)))
                      (funcall one-f i)))))
    (make-instance 'range :size (+ (size one) (size other)) :transformation new-f)))

(defmethod fmap (f (range range))
  (make-instance 'range :size (size range) 
                 :transformation (lambda (i) (funcall f (funcall (transformation range) i)))))

(defun merge-ranges (f range start end)
  (if (= start end)
      (funcall f (funcall (transformation range) start))
      (let* ((mid (floor (+ start end) 2))
             (left (merge-ranges f range start mid))
             (right (merge-ranges f range (+ mid 1) end)))
        (append-ranges left right))))

(defmethod flatmap (f (range range))
  (merge-ranges f range 0 (- (size range) 1))
  ;; (loop for i from 0 to (- (size range) 1)
  ;;    for new-range = (funcall f (funcall (transformation range) i))
  ;;    for result = new-range then (append-ranges result new-range)
  ;;    finally (return result))
  )

(defun index (range i)
  (funcall (transformation range) i))

(defun reduce-range (op range)
  (loop for i from 0 to (- (size range) 1)
     for e = (funcall (transformation range) i)
     for result = e then (funcall op result e)
     finally (return result)))
