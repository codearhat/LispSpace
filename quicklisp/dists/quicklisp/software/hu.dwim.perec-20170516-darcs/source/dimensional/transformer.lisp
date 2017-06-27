;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;;;
;;; Unused reader and writer

(def function h-unused-reader (rdbms-values index)
  (if (eq :null (elt rdbms-values index))
      +h-unused-slot-marker+
      +type-error-marker+))

(def function h-unused-writer (slot-value rdbms-values index)
  (if (h-unused-slot-marker-p slot-value)
      (setf (elt rdbms-values index) :null)
      +type-error-marker+))
