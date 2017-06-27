;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.perec.sqlite
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "Sqlite backend for hu.dwim.perec."
  :depends-on (:hu.dwim.perec
               :hu.dwim.rdbms.sqlite)
  :components ((:module "source"
                :components ((:module "backend"
                              :components ((:file "sqlite")))))))

(defmethod perform :after ((op hu.dwim.asdf:develop-op) (system (eql (find-system :hu.dwim.perec.sqlite))))
  (let ((database-variable (read-from-string "hu.dwim.rdbms::*database*")))
    (unless (boundp database-variable)
      (setf (symbol-value database-variable)
            (symbol-value (read-from-string "hu.dwim.perec.test::*sqlite-database*")))
      (warn "The global value of *database* was initialized.")))
  (eval (let ((*package* (find-package :hu.dwim.perec)))
          (read-from-string "(setf *compiled-query-cache* (make-compiled-query-cache))")))
  (warn "The global value of *compiled-query-cache* was initialized."))
