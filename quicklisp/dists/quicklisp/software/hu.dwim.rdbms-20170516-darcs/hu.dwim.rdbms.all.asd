;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.rdbms.all
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :package-name :hu.dwim.rdbms
  :description "All backends for hu.dwim.rdbms."
  :depends-on (:hu.dwim.rdbms.oracle
               :hu.dwim.rdbms.postgresql
               :hu.dwim.rdbms.sqlite))
