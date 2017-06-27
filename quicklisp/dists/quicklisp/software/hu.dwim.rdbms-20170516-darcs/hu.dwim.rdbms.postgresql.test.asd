;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.rdbms.postgresql.test
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :package-name :hu.dwim.rdbms.test
  :test-name "TEST/POSTGRESQL"
  :depends-on (:hu.dwim.rdbms.postgresql
               :hu.dwim.rdbms.test)
  :components ((:module "test"
                :components ((:file "postgresql")))))

(defmethod hu.dwim.asdf::call-in-system-environment ((operation load-op) (system (eql (find-system :hu.dwim.rdbms.postgresql.test))) function)
  (progv
      (list (read-from-string "hu.dwim.rdbms:*database*"))
      (list (eval (read-from-string "(make-instance 'hu.dwim.rdbms.postgresql:postgresql)")))
    (call-next-method)))
