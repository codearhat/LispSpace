;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;;;
;;; Time dimension

(def (dimension e) time
  :type timestamp
  :inherit :ascending
  :bind-default-coordinate #f
  :default-coordinate-begin (transaction-timestamp)
  :default-coordinate-end (transaction-timestamp)
  :minimum-coordinate +beginning-of-time+
  :maximum-coordinate +end-of-time+)

;;;;;;
;;; Validity dimension

(def (dimension e) validity
  :type timestamp
  :ordered #t
  :default-coordinate-begin +beginning-of-time+
  :default-coordinate-end +end-of-time+
  :minimum-coordinate +beginning-of-time+
  :maximum-coordinate +end-of-time+)

;;;;;;
;;; Enumerated dimension

(def (dimension e) enumerated :type symbol)
