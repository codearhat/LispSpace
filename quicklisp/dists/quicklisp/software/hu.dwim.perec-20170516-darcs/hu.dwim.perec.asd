;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.perec
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "RDBMS based persistent CLOS, an object relational mapping (ORM)."
  :depends-on (:babel
               :contextl
               :cl-containers
               :cl-ppcre
               :hu.dwim.common
               :hu.dwim.computed-class
               :hu.dwim.def+hu.dwim.common
               :hu.dwim.def+hu.dwim.delico
               :hu.dwim.def+contextl
               :hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.logger
               :hu.dwim.rdbms
               :hu.dwim.serializer
               :hu.dwim.syntax-sugar
               :hu.dwim.syntax-sugar/lambda-with-bang-args
               :hu.dwim.util/mop
               :hu.dwim.walker
               :ironclad
               :local-time
               :metacopy-with-contextl
               :parse-number)
  :components ((:module "source"
                :components ((:file "package")
                             (:file "logger" :depends-on ("package"))
                             (:module "util"
                              :depends-on ("package" "logger")
                              :components ((:file "duplicates")
                                           (:file "logic")))
                             (:module "persistence"
                              :depends-on ("util")
                              :components ((:file "api")
                                           (:file "common")
                                           (:file "transaction")
                                           (:file "oid" :depends-on ("common" "transaction"))
                                           (:file "table" :depends-on ("common" "oid"))
                                           (:file "type" :depends-on ("table" "oid"))
                                           (:file "class" :depends-on ("table" "type" "api"))
                                           (:file "standard-type" :depends-on ("type" "class"))
                                           (:file "association" :depends-on ("class"))
                                           (:file "mop" :depends-on ("class" "association"))
                                           (:file "object" :depends-on ("api" "mop" "transaction"))
                                           (:file "instance-cache")
                                           (:file "store" :depends-on ("oid" "class" "object" "standard-type"))
                                           (:file "slot-value" :depends-on ("store"))
                                           (:file "persistent" :depends-on ("oid" "slot-value"))
                                           (:file "transformer" :depends-on ("type" "standard-type"))
                                           (:file "set")
                                           (:file "association-end-set" :depends-on ("object"))
                                           (:file "copy" :depends-on ("object"))
                                           (:file "export" :depends-on ("object"))))
                             (:module "query"
                              :depends-on ("util" "persistence")
                              :components ((:file "conditions")
                                           (:file "api")
                                           (:file "copy")
                                           (:file "macro")
                                           (:file "syntax" :depends-on ("copy"))
                                           (:file "runtime" :depends-on ("syntax"))
                                           (:file "scroll")
                                           (:file "result-set" :depends-on ("scroll"))
                                           (:file "cache" :depends-on ("query"))
                                           (:file "query" :depends-on ("conditions" "syntax" "api"))
                                           (:file "type" :depends-on ("syntax" "query"))
                                           (:file "sql" :depends-on ("type"))
                                           (:file "partial-eval" :depends-on ("query"))
                                           (:file "mapping" :depends-on ("query" "sql" "partial-eval" "runtime"))
                                           (:file "plan" :depends-on ("mapping" "result-set" "runtime"))
                                           (:file "compiler" :depends-on ("type" "copy" "plan" "macro"))
                                           (:file "constraint" :depends-on ("compiler"))))
                             (:module "dimensional"
                              :depends-on ("persistence" "query")
                              :components ((:file "common")
                                           (:file "dimension")
                                           (:file "type")
                                           (:file "set")
                                           (:file "coordinate")
                                           (:file "range" :depends-on ("coordinate"))
                                           (:file "coordinate-set" :depends-on ("dimension" "range" "set"))
                                           (:file "value" :depends-on ("dimension" "set"))
                                           (:file "standard-dimension" :depends-on ("dimension" "value"))
                                           (:file "class" :depends-on ("type" "dimension"))
                                           (:file "association" :depends-on ("class" "value"))
                                           (:file "mop" :depends-on ("class" "association"))
                                           (:file "object" :depends-on ("class"))
                                           (:file "store" :depends-on ("value" "class" "association" "object" "association-end-set"))
                                           (:file "cache" :depends-on ("value" "class" "association"))
                                           (:file "slot-value" :depends-on ("store" "association" "cache"))
                                           (:file "transformer" :depends-on ("type"))
                                           (:file "association-end-set" :depends-on ("value"))
                                           (:file "instance-cache" :depends-on ("slot-value"))))))))

(defmethod perform :after ((op hu.dwim.asdf:develop-op) (system (eql (find-system :hu.dwim.perec))))
  (eval (let ((*package* (find-package :hu.dwim.perec)))
          (read-from-string "(setf *compiled-query-cache* (make-compiled-query-cache))")))
  (warn "The global value of *compiled-query-cache* was initialized."))
