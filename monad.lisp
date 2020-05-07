(defpackage :monad
  (:use :cl)
  (:export :mdo
           :fmap
           :flatmap))

(in-package :monad)

(defgeneric flatmap (fun obj))

(defgeneric fmap (fun obj))

(defun transform-clause (&optional (seen-unwrap nil))
  (lambda (acc clause)
    (cond ((eq (car clause) 'let)
           (append `(let ,(cdr clause)) (list acc)))
          ((not seen-unwrap)
           (setf seen-unwrap t)
           `(fmap (lambda (,(car clause)) ,acc) ,(cadr clause)))
          (t `(flatmap (lambda (,(car clause)) ,acc) ,(cadr clause))))))

(defun check-clauses (exps)
  (when (not (every (lambda (x) (or (eq (car x) 'let)
                                    (and (symbolp (car x))
                                         (= (length x) 2))))
                    (butlast exps)))
    (error "Every clause in mdo must begin with <- or let"))
  (when (not (string= (caar (last exps)) "YIELD"))
    (error "The final clause in mdo must be a yield"))
  (when (not (= (length (car (last exps))) 2))
    (error "The yield clause in mdo must only contain one candidate result")))

(defun transform-mdo (exps)
  (let ((reversed (reverse exps)))
    (reduce (transform-clause) (cdr reversed) :initial-value (cadr (car reversed)))))

(defmacro mdo (&rest exps)
  (check-clauses exps)
  (transform-mdo exps))
