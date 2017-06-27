;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.perec.sqlite.test
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :package-name :hu.dwim.perec.test
  :test-name "TEST/SQLITE"
  :description "Test suite for hu.dwim.perec with Sqlite backend."
  :depends-on (:hu.dwim.perec.test
               :hu.dwim.perec.sqlite)
  :components ((:module "test"
                :components ((:module "backend"
                              :components ((:file "sqlite")))))))
