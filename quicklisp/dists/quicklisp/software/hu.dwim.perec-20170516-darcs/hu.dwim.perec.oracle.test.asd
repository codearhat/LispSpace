;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.perec.oracle.test
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :package-name :hu.dwim.perec.test
  :test-name "TEST/ORACLE"
  :description "Test suite for hu.dwim.perec with Oracle backend."
  :depends-on (:hu.dwim.perec.test
               :hu.dwim.perec.oracle)
  :components ((:module "test"
                :components ((:module "backend"
                              :components ((:file "oracle")))))))
