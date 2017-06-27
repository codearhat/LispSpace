;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.syntax-sugar
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "Various syntax extensions."
  :depends-on (:hu.dwim.common)
  :components ((:module "source"
                :components ((:file "duplicates" :depends-on ("package"))
                             (:file "feature-cond" :depends-on ("one-liners"))
                             (:file "case-preserving" :depends-on ("syntax-sugar"))
                             (:file "number" :depends-on ("syntax-sugar"))
                             (:file "one-liners" :depends-on ("syntax-sugar"))
                             (:file "package")
                             (:file "quasi-quote" :depends-on ("one-liners"))
                             (:file "readtime-wrapper" :depends-on ("one-liners"))
                             (:file "string-quote" :depends-on ("one-liners"))
                             (:file "syntax-sugar" :depends-on ("duplicates"))))))

(defsystem :hu.dwim.syntax-sugar/lambda-with-bang-args
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :depends-on (:hu.dwim.syntax-sugar
               :hu.dwim.walker)
  :components ((:module "source"
                :components ((:file "lambda-with-bang-args")))))

(defsystem :hu.dwim.syntax-sugar/unicode
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "Various unicode syntax extensions."
  :depends-on (:hu.dwim.syntax-sugar)
  :components ((:module "source"
                :components ((:module "unicode"
                              :components ((:file "package")
                                           (:file "one-liners" :depends-on ("package"))))))))
