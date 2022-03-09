(defpackage :utils
  (:use :cl)
  (:export :with-gensyms
           :dorange
           :my-dolist
           :aif
           :it
           :alambda
           :nlet))

(in-package :utils)

(defmacro with-gensyms (symbols-list &body body)
  (let ((bindings (mapcar (lambda (s) `(,s (gensym))) symbols-list)))
    `(let ,bindings
       ,@body)))

(defmacro dorange ((var a b &optional (step 1)) &body body)
  (with-gensyms (stop-value step-value)
    `(do ((,stop-value ,b)
          (,step-value ,step)
          (,var ,a (+ ,var ,step-value)))
         ((>= ,var ,stop-value))
       ,@body)))

(defmacro my-dolist ((var list-form &optional result-form) &body body)
  (with-gensyms (list-head check)
    `(prog (,var (,list-head ,list-form))
           ,check
           (if (null ,list-head)
               (return ,result-form))
           (setq ,var (car ,list-head))
           (progn ,@body)
           (setq ,list-head (cdr ,list-head))
           (go ,check))))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it
         ,then-form
         ,else-form)))

(defmacro alambda (lambda-list &body body)
  `(labels ((self ,lambda-list ,@body))
     #'self))

(defmacro nlet (name bindings &rest body)
  `(labels ((,name ,(mapcar #'car bindings) ,@body))
     (,name ,@(mapcar #'cadr bindings))))
