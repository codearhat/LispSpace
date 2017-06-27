;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.perec+hu.dwim.quasi-quote.xml
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :depends-on (:hu.dwim.perec
               :hu.dwim.quasi-quote.xml)
  :components ((:module "integration"
                :components ((:file "quasi-quote.xml")))))
