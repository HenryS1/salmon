(defpackage :monad
  (:use :cl)
  (:export :mdo
           :fmap
           :flatmap))

(in-package :monad)

(defgeneric flatmap (fun obj))

(defgeneric fmap (fun obj))

(defmethod fmap (fun (l list))
  (mapcar fun l))

(defmethod flatmap (fun (l list))
  (reduce #'nconc (mapcar fun l)))

(defmethod fmap (fun (v vector))
  (map 'vector fun v))

(defmethod fmap (fun (s try:success))
  (try:success (try:value s)))

(defmethod flatmap (fun (s try:success))
  (funcall fun (try:value s)))

(defmethod fmap (fun (f try:failure)) f)

(defmethod flatmap (fun (f try:failure)) f)

(defmethod flatmap (fun (v vector))
  (let* ((vs (fmap fun v))
         (result (make-array (reduce (lambda (a b) (+ a (length b))) vs :initial-value 0))))
    (loop with i = 0
       for v across vs
       do (loop for el across v
             do (setf (aref result i) el)
               (incf i)))
    result))

(defmethod fmap (fun (maybe:nothing maybe:nothing)) maybe:nothing)

(defmethod fmap (fun (maybe:just maybe:just))
  (maybe:just (funcall fun (maybe:value maybe:just))))

(defmethod flatmap (fun (maybe:nothing maybe:nothing)) maybe:nothing)

(defmethod flatmap (fun (maybe:just maybe:just))
  (funcall fun (maybe:value maybe:just)))

(defun transform-clause (&optional (seen-unwrap nil))
  (lambda (acc clause)
    (cond ((and (eq (car clause) '<-) 
                (not seen-unwrap))
           (setf seen-unwrap t)
           `(fmap (lambda (,(cadr clause)) ,acc) ,(caddr clause)))
          ((eq (car clause) '<-)
           `(flatmap (lambda (,(cadr clause)) ,acc) ,(caddr clause)))
          (t (append clause (list acc))))))

(defun check-clauses (exps)
  (when (not (every (lambda (x) (or (eq (car x) '<-) (eq (car x) 'let))) (butlast exps)))
    (error "Every clause must begin with <- or let"))
  (when (not (eq (caar (last exps)) 'yield))
    (error "The final clause must be a yield"))
  (when (not (= (length (car (last exps))) 2))
    (error "The yield clause must only contain one candidate result")))

(defun transform-mdo (exps)
  (let ((reversed (reverse exps)))
    (reduce (transform-clause) (cdr reversed) :initial-value (cadr (car reversed)))))

(defmacro mdo (&rest exps)
  (check-clauses exps)
  (transform-mdo exps))
