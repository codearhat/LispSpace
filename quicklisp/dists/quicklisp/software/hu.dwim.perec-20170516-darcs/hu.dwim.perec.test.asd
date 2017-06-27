;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.perec.test
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :description "Test suite for hu.dwim.perec"
  :depends-on (:hu.dwim.perec+swank
               :hu.dwim.perec+iolib
               :hu.dwim.perec+hu.dwim.quasi-quote.xml
               :hu.dwim.util.test)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "suite" :depends-on ("package"))
                             (:module "persistence"
                              :depends-on ("suite")
                              :components ((:file "1-1-association")
                                           (:file "1-n-association" :depends-on ("1-1-association"))
                                           (:file "cache" :depends-on ("1-n-association" "1-1-association"))
                                           (:file "canonical")
                                           (:file "change-class")
                                           (:file "export")
                                           (:file "inheritance")
                                           (:file "instance")
                                           (:file "m-n-association")
                                           (:file "persistence")
                                           (:file "purge")
                                           (:file "reference")
                                           #+nil ;; TODO: broken right now
                                           (:file "set")
                                           (:file "table")
                                           (:file "transaction")
                                           (:file "transformer")
                                           (:file "type")))
                             (:module "query"
                              :depends-on ("suite")
                              :components ((:file "1-1-association" :depends-on ("suite" "association"))
                                           (:file "1-n-association" :depends-on ("suite" "association"))
                                           (:file "aggregate" :depends-on ("suite"))
                                           (:file "association" :depends-on ("suite"))
                                           (:file "cache" :depends-on ("association"))
                                           (:file "embedded-sql" :depends-on ("suite"))
                                           (:file "expression" :depends-on ("suite"))
                                           (:file "forum" :depends-on ("suite"))
                                           (:file "group-by" :depends-on ("suite"))
                                           (:file "having" :depends-on ("suite"))
                                           (:file "limit" :depends-on ("suite"))
                                           (:file "m-n-association" :depends-on ("suite" "association"))
                                           (:file "order-by" :depends-on ("suite"))
                                           (:file "partial-eval" :depends-on ("suite"))
                                           (:file "polymorph" :depends-on ("suite"))
                                           (:file "purge" :depends-on ("suite"))
                                           (:file "scroll" :depends-on ("suite"))
                                           (:file "select-list" :depends-on ("suite"))
                                           (:file "subselect" :depends-on ("suite"))
                                           (:file "suite")
                                           (:file "table-ref" :depends-on ("suite"))
                                           (:file "type" :depends-on ("suite"))
                                           (:file "update" :depends-on ("suite"))))
                             (:module "dimensional"
                              :depends-on ("suite")
                              :components ((:file "1-1-association" :depends-on ("complex" "association"))
                                           (:file "1-n-association" :depends-on ("complex" "association"))
                                           (:file "association")
                                           (:file "cache")
                                           (:file "complex")
                                           (:file "dimensional")
                                           (:file "m-n-association" :depends-on ("complex" "association"))
                                           (:file "partial-timestamp")
                                           (:file "slot" :depends-on ("complex"))
                                           (:file "time")
                                           (:file "validity")
                                           (:file "value")))))))
