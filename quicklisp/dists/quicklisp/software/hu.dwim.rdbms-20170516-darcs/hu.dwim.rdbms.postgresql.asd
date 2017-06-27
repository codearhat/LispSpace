;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.rdbms.postgresql
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :package-name :hu.dwim.rdbms
  :description "Postgresql backend for hu.dwim.rdbms."
  :depends-on (:cl-postgres+local-time
               :hu.dwim.rdbms)
  :components ((:module "source"
                :components ((:module "postgresql"
                              :components ((:file "package")
                                           (:file "database" :depends-on ("package"))
                                           (:file "syntax" :depends-on ("database"))
                                           (:file "backend" :depends-on ("database"))
                                           (:file "type" :depends-on ("database"))
                                           (:file "ddl" :depends-on ("database"))))))))

(defmethod perform :after ((op hu.dwim.asdf:develop-op) (system (eql (find-system :hu.dwim.rdbms.postgresql))))
  (let ((database-variable (read-from-string "hu.dwim.rdbms::*database*")))
    (unless (boundp database-variable)
      (setf (symbol-value database-variable)
            (symbol-value (read-from-string "hu.dwim.rdbms.test::*postgresql-database*"))))))
