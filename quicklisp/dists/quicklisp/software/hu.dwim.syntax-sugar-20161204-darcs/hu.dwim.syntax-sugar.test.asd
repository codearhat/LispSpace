;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.syntax-sugar.test
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :depends-on (:hu.dwim.stefil+hu.dwim.def+swank
               :hu.dwim.syntax-sugar
               :hu.dwim.syntax-sugar/lambda-with-bang-args
               :hu.dwim.syntax-sugar/unicode)
  :components ((:module "test"
                :components ((:file "feature-cond" :depends-on ("suite"))
                             (:file "lambda" :depends-on ("suite"))
                             (:file "package")
                             (:file "quasi-quote" :depends-on ("suite"))
                             (:file "readtime-wrapper" :depends-on ("suite"))
                             (:file "sharp-l" :depends-on ("lambda"))
                             (:file "string-quote" :depends-on ("suite"))
                             (:file "suite" :depends-on ("package"))))))
