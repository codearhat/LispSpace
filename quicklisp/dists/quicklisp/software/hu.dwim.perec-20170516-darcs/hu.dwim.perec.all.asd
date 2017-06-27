;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.perec.all
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "All backends for hu.dwim.perec."
  :depends-on (:hu.dwim.perec.oracle
               :hu.dwim.perec.postgresql
               :hu.dwim.perec.sqlite))

(defmethod perform :after ((op hu.dwim.asdf:develop-op) (system (eql (find-system :hu.dwim.perec.all))))
  (eval (let ((*package* (find-package :hu.dwim.perec)))
          (read-from-string "(setf *compiled-query-cache* (make-compiled-query-cache))")))
  (warn "The global value of *compiled-query-cache* was initialized."))
