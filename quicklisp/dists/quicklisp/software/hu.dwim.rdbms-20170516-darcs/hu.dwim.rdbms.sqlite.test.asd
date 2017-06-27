;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.rdbms.sqlite.test
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :package-name :hu.dwim.rdbms.test
  :test-name "TEST/SQLITE"
  :depends-on (:hu.dwim.rdbms.sqlite
               :hu.dwim.rdbms.test)
  :components ((:module "test"
                :components ((:file "sqlite")))))

(defmethod hu.dwim.asdf::call-in-system-environment ((operation load-op) (system (eql (find-system :hu.dwim.rdbms.sqlite.test))) function)
  (progv
      (list (read-from-string "hu.dwim.rdbms:*database*"))
      (list (eval (read-from-string "(make-instance 'hu.dwim.rdbms.sqlite:sqlite)")))
    (call-next-method)))
