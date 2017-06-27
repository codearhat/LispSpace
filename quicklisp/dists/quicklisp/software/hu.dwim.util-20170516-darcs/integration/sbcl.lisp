;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2015 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

(with-muffled-redefinition-warnings
  (def (with-macro* e) with-profiling ()
    (sb-sprof:with-profiling ()
      (-body-))))
