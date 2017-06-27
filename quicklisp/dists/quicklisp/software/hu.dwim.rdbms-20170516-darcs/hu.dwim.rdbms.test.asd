;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.rdbms.test
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :depends-on (:hu.dwim.rdbms
               :hu.dwim.stefil+hu.dwim.def+swank)
  :components ((:module "test"
                :components ((:file "basic" :depends-on ("suite"))
                             (:file "package")
                             (:file "suite" :depends-on ("package"))
                             (:file "syntax" :depends-on ("suite"))
                             (:file "type" :depends-on ("suite"))))))
