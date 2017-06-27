;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.rdbms
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "Relational database independent RDBMS and SQL abstractions."
  :depends-on (:babel
               :hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.logger
               :hu.dwim.syntax-sugar
               :hu.dwim.syntax-sugar/lambda-with-bang-args
               :hu.dwim.util
               :hu.dwim.util/error-handling
               :hu.dwim.walker
               :ironclad
               :local-time)
  :components ((:module "source"
                :components ((:file "conditions" :depends-on ("package"))
                             (:file "logger" :depends-on ("package"))
                             (:file "package")
                             (:module "generic"
                              :depends-on ("conditions" "logger" "variables")
                              :components ((:file "cursor" :depends-on ("transaction"))
                                           (:file "database")
                                           (:file "ddl" :depends-on ("transaction"))
                                           (:file "dml" :depends-on ("transaction"))
                                           (:file "transaction" :depends-on ("database"))))
                             (:module "syntax"
                              :depends-on ("generic")
                              :components ((:file "alter-table" :depends-on ("create-table" "constraint"))
                                           (:file "constraint" :depends-on ("syntax"))
                                           (:file "create-table" :depends-on ("syntax" "expression"))
                                           (:file "delete" :depends-on ("syntax"))
                                           (:file "drop-table" :depends-on ("syntax"))
                                           (:file "expression" :depends-on ("syntax"))
                                           (:file "format")
                                           (:file "index" :depends-on ("syntax"))
                                           (:file "insert" :depends-on ("syntax"))
                                           (:file "lock" :depends-on ("syntax"))
                                           (:file "reader-macro")
                                           (:file "select" :depends-on ("syntax" "expression"))
                                           (:file "sequence" :depends-on ("syntax"))
                                           (:file "sexp" :depends-on ("syntax" "expression"))
                                           (:file "syntax" :depends-on ("format"))
                                           (:file "type" :depends-on ("syntax"))
                                           (:file "update" :depends-on ("syntax"))))
                             (:file "variables" :depends-on ("package"))))))
