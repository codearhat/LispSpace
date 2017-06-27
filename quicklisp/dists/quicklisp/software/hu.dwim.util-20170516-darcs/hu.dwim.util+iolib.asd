;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.util+iolib
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "Integration of hu.dwim.util with iolib."
  :depends-on (:hu.dwim.util
               :iolib/os
               :iolib/pathnames
               :iolib/syscalls)
  :components ((:module "integration"
                :components ((:file "iolib")))))
