;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.rdbms.all.test
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :package-name :hu.dwim.rdbms.test
  :depends-on (:hu.dwim.rdbms.oracle.test
               :hu.dwim.rdbms.postgresql.test
               :hu.dwim.rdbms.sqlite.test))
