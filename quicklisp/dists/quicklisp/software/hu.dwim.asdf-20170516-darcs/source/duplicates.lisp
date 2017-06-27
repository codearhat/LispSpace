;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.asdf)

(defun if-symbol-exists (package name)
  (if (and (find-package (string package))
           (find-symbol (string name) (string package)))
      '(:and)
      '(:or)))

;; this exists in :hu.dwim.util (in a slightly different form)
(defun call-with-muffled-boring-compiler-warnings (thunk)
  (handler-bind (#+sbcl(sb-ext:compiler-note #'muffle-warning)
                 ;; NOTE: muffle these warnings to reduce compilation noise, tests already cover interesting cases
                 #+sbcl(sb-kernel:undefined-alien-style-warning #'muffle-warning))
    (funcall thunk)))

(defmacro with-muffled-boring-compiler-warnings (&body body)
  `(locally
       (declare #+sbcl(sb-ext:muffle-conditions style-warning sb-ext:compiler-note))
     (call-with-muffled-boring-compiler-warnings (lambda () ,@body))))

