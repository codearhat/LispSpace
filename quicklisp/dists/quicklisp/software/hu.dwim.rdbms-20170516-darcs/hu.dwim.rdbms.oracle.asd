;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.rdbms.oracle
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :package-name :hu.dwim.rdbms
  :description "Oracle backend for hu.dwim.rdbms."
  :depends-on (:cffi
               :hu.dwim.rdbms)
  :components ((:module "source"
                :components ((:module "oracle"
                              :components ((:file "package")
                                           (:file "cffi" :depends-on ("package"))
                                           (:file "database" :depends-on ("cffi"))
                                           (:file "syntax" :depends-on ("database"))
                                           (:file "ddl" :depends-on ("database"))
                                           (:file "cffi-util" :depends-on ("database"))
                                           (:file "conversion" :depends-on ("cffi-util"))
                                           (:file "type" :depends-on ("database"))
                                           (:file "backend" :depends-on ("cffi-util"))))))))

(defmethod perform :after ((op hu.dwim.asdf:develop-op) (system (eql (find-system :hu.dwim.rdbms.oracle))))
  (let ((database-variable (read-from-string "hu.dwim.rdbms::*database*")))
    (unless (boundp database-variable)
      (setf (symbol-value database-variable)
            (symbol-value (read-from-string "hu.dwim.rdbms.test::*oracle-database*"))))))
