;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2010 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.logger)

(def function logger-lookup-hook (form)
  (when (typep form 'logger-name)
    (bind ((possible-logger-names (remove nil (list form (get form 'logger)))))
      (dolist (logger-name possible-logger-names)
        (awhen (find-logger logger-name :otherwise nil)
          (return (values it t)))))))

(when (boundp 'swank::*inspector-lookup-hooks*)
  (pushnew 'logger-lookup-hook (symbol-value 'swank::*inspector-lookup-hooks*)))
