;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

(def (class* e) sqlite/perec (database-mixin hu.dwim.rdbms.sqlite:sqlite)
  ())
