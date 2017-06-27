;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.syntax-sugar.documentation
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.documentation-system"
  :depends-on (:hu.dwim.syntax-sugar.test
               :hu.dwim.presentation)
  :components ((:module "documentation"
                :components ((:file "package")
                             (:file "syntax-sugar" :depends-on ("package"))))))
