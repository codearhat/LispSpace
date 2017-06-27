;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(unless (or #+asdf3 (uiop:version<= "2.31.1" (asdf-version)))
  (error "You need ASDF >= 2.31.1 to load this system correctly."))

(defsystem :hu.dwim.common
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "An extended Common Lisp package to the general needs of other hu.dwim systems."
  :depends-on (:alexandria
               :anaphora
               :closer-mop
               :hu.dwim.common-lisp
               :iterate
               :metabang-bind)
  :components ((:module "source"
                :components ((:file "common" :depends-on ("package"))
                             (:file "package")))))
