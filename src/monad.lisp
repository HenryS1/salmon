(defpackage :monad
  (:use :cl)
  (:export :mdo
           :fmap
           :flatmap))

(in-package :monad)

(defgeneric flatmap (fun obj))

(defgeneric fmap (fun obj))

(defun ignore-underscore (name) 
  (if (equal (symbol-name name) (symbol-name '_)) (list `(declare (ignore ,name))) ()))

(defun transform-clause (&optional (seen-unwrap nil))
  (lambda (acc clause)
    (cond ((eq (car clause) 'let)
           (append `(let ,(cdr clause) 
                      ,@(ignore-underscore (caadr clause))) (list acc)))
          ((equal (symbol-name (car clause)) (symbol-name 'handle))
           `(handler-case 
                ,acc 
              (,(cadr clause) ,(caddr clause) ,@(cdddr clause))))
          ((equal (symbol-name (car clause)) (symbol-name 'with))
           (if (not seen-unwrap)
               (progn (setf seen-unwrap t)
                      `(fmap (lambda (,(caddr clause)) 
                               ,@(ignore-underscore (caddr clause))
                              (unwind-protect 
                                   ,acc
                                (funcall ,(cadr clause) ,(caddr clause))))
                            (progn ,@(cdddr clause))))
               `(flatmap (lambda (,(caddr clause))
                                  ,@(ignore-underscore (caddr clause))
                            (unwind-protect
                                 ,acc
                              (funcall ,(cadr clause) ,(caddr clause))))
                          (progn ,@(cdddr clause)))))
          ((equal (symbol-name (car clause)) (symbol-name 'clean-on-error))
           (let ((e (gensym)))
             (if (not seen-unwrap)
                 (progn (setf seen-unwrap t)
                        `(fmap (lambda (,(caddr clause)) 
                                 ,@(ignore-underscore (caddr clause))
                                 (handler-case 
                                     ,acc
                                   (error (,e)
                                     (funcall ,(cadr clause) ,(caddr clause))
                                     (error ,e))))
                               (progn ,@(cdddr clause))))
                 `(flatmap (lambda (,(caddr clause))
                             ,@(ignore-underscore (caddr clause))
                             (handler-case
                                 ,acc
                               (error (,e) 
                                 (funcall ,(cadr clause) ,(caddr clause))
                                 (error ,e))))
                           (progn ,@(cdddr clause))))))
          ((not seen-unwrap)
           (setf seen-unwrap t)
           `(fmap (lambda (,(car clause)) 
                    ,@(ignore-underscore (car clause))
                    ,acc) 
                  (progn ,@(cdr clause))))
          (t `(flatmap (lambda (,(car clause))
                         ,@(ignore-underscore (car clause))
                         ,acc) (progn ,@(cdr clause)))))))

(defun check-clauses (exps)
  (when (not (string= (caar (last exps)) "YIELD"))
    (error "The final clause in mdo must be a yield")))

(defun transform-mdo (exps)
  (let ((reversed (reverse exps)))
    (reduce (transform-clause) (cdr reversed) :initial-value `(progn ,@(cdr (car reversed))))))

(defmacro mdo (&rest exps)
  (check-clauses exps)
  (transform-mdo exps))
