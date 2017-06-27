;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.perec.all.test
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :package-name :hu.dwim.perec.test
  :description "Test suite for all backends of hu.dwim.perec."
  :depends-on (:hu.dwim.perec.oracle.test
               :hu.dwim.perec.postgresql.test
               :hu.dwim.perec.sqlite.test))
