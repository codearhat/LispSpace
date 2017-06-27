;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2013 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.logger+iolib
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :depends-on (:hu.dwim.logger
               ;; we force the loading of hu.dwim.util+iolib to redefine GET-MONOTONIC-TIME with a proper one from iolib
               :hu.dwim.util+iolib))
