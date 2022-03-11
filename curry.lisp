(defpackage :curry
  (:use :cl))

(in-package :curry)

(defun curry (stream subchar arg)
  (declare (ignore subchar arg))
  (let* ((form (read stream t nil t))
         (func (car form))
         (curry-args (cdr form)))
    `(lambda (&rest args)
       (apply #',func ,@curry-args args))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character #\# #\$ #'curry))
