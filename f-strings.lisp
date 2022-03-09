(defpackage :f-strings
  (:use :cl))

(in-package :f-strings)

(defun read-closing-brace (stream char)
  (declare (ignore stream))
  (error "Character ~S shouldn't be read directly" char))

(defun parse-f-string (stream subchar arg)
  (declare (ignore subchar arg))
  (let* ((f-string (read stream t nil t))
         (forms nil)
         (control-string
           (with-input-from-string (in-stream f-string)
             (with-output-to-string (out-stream)
               (let ((*standard-input* in-stream)
                     (*standard-output* out-stream)
                     (delim-readtable (copy-readtable)))
                 (set-macro-character #\} #'read-closing-brace nil delim-readtable)
                 (loop for c = (read-char nil nil nil)
                       while c
                       do (case c
                            (#\{ (cond
                                   ((eql (peek-char nil nil nil nil) #\{)
                                    (read-char nil nil nil)
                                    (princ #\{))
                                   (t
                                    (let ((*readtable* delim-readtable))
                                      (push (read-delimited-list #\}) forms))
                                    (princ "~A"))))
                            (#\} (cond
                                   ((eql (peek-char nil nil nil nil) #\})
                                    (read-char nil nil nil)
                                    (princ #\}))
                                   (t (error "Unmatched } in f-string"))))
                            (#\~ (princ "~~"))
                            (otherwise (write-char c)))))))))
    (setf forms (nreverse forms))
    (map-into forms
              (lambda (x) (cons 'progn x))
              forms)
    `(format nil ,control-string ,@forms)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character #\# #\f #'parse-f-string))
