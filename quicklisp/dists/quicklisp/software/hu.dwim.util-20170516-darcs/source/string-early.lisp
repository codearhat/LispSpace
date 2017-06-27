;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; String concatenation

(def (function eo) string+ (&rest args)
  ;; don't inline, otherwise the compiler macro is kicked
  (apply #'concatenate 'string args))

(def compiler-macro string+ (&rest args)
  `(concatenate 'string ,@args))

(def (function eo) join-strings (strings &optional separator)
  (with-output-to-string (string)
    (iter (for el :in-sequence strings)
          (when (and separator
                     (not (first-time-p)))
            (princ separator string))
          (write-string el string))))
