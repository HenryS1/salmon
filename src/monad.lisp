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

(defun transform-clause (acc clause)
  (destructuring-bind (clauses . seen-unwrap) acc
    (cond ((eq (car clause) 'let)
           (cons (append `(let ,(cdr clause) 
                            ,@(ignore-underscore (caadr clause))) (list clauses))
                 seen-unwrap))
          ((equal (symbol-name (car clause)) (symbol-name 'handle))
           (cons `(handler-case 
                      ,clauses 
                    (,(cadr clause) ,(caddr clause) ,@(cdddr clause)))
                 seen-unwrap))
          ((equal (symbol-name (car clause)) (symbol-name 'with))
           (if (not seen-unwrap)
               (cons `(fmap (lambda (,(caddr clause)) 
                              ,@(ignore-underscore (caddr clause))
                              (unwind-protect 
                                   ,clauses
                                (funcall ,(cadr clause) ,(caddr clause))))
                            (progn ,@(cdddr clause)))
                     t)
               (cons `(flatmap (lambda (,(caddr clause))
                                 ,@(ignore-underscore (caddr clause))
                                 (unwind-protect
                                      ,clauses
                                   (funcall ,(cadr clause) ,(caddr clause))))
                               (progn ,@(cdddr clause)))
                     seen-unwrap)))
          ((equal (symbol-name (car clause)) (symbol-name 'clean-on-error))
           (let ((e (gensym)))
             (if (not seen-unwrap)
                 (cons `(fmap (lambda (,(caddr clause)) 
                                (handler-case 
                                    ,clauses
                                  (error (,e)
                                    (funcall ,(cadr clause) ,(caddr clause))
                                    (error ,e))))
                              (progn ,@(cdddr clause)))
                       t)
                 (cons `(flatmap (lambda (,(caddr clause))
                                   (handler-case
                                       ,clauses
                                     (error (,e) 
                                       (funcall ,(cadr clause) ,(caddr clause))
                                       (error ,e))))
                                 (progn ,@(cdddr clause)))
                       seen-unwrap))))
          ((not seen-unwrap)
           (cons `(fmap (lambda (,(car clause)) 
                          ,@(ignore-underscore (car clause))
                          ,clauses) 
                        (progn ,@(cdr clause)))
                 t))
          (t (cons `(flatmap (lambda (,(car clause))
                               ,@(ignore-underscore (car clause))
                               ,clauses) (progn ,@(cdr clause)))
                   seen-unwrap)))))

(defun check-clauses (exps)
  (when (not (string= (caar (last exps)) "YIELD"))
    (error "The final clause in mdo must be a yield")))

(defun transform-mdo (exps)
  (let ((reversed (reverse exps)))
    (car (reduce #'transform-clause (cdr reversed) 
                 :initial-value (cons `(progn ,@(cdr (car reversed))) nil)))))

(defmacro mdo (&rest exps)
  (check-clauses exps)
  (transform-mdo exps))
