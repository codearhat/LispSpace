;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :hu.dwim.perec
  (:use :babel
        :cl-containers
        :cl-ppcre
        :hu.dwim.asdf
        :hu.dwim.common
        :hu.dwim.computed-class
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.logger
        :hu.dwim.rdbms
        :hu.dwim.serializer
        :hu.dwim.syntax-sugar
        :hu.dwim.util
        :hu.dwim.walker
        :local-time
        :metacopy-with-contextl
        :parse-number)

  (:shadow #:name
           #:set
           #:form
           #:children
           #:variable
           #:class-slots
           #:transaction
           #:create-temporary-table ; TODO resolve by renaming to something. hu.dwim.rdbms exports this.
           #:float-type
           #:class-name-of)

  (:shadowing-import-from :cl-containers
                          #:dequeue
                          #:enqueue
                          #:parent)

  (:shadowing-import-from :hu.dwim.rdbms
                          #:update-index
                          #:insert-record)

  (:shadowing-import-from :hu.dwim.def
                          #:iterator)

  (:shadowing-import-from :iterate
                          #:finish)

  (:shadowing-import-from :local-time
                          #:time)

  (:export ;; defining persistent classes
           #:defpclass
           #:defpclass*
           #:find-class
           #:find-persistent-class
           #:ensure-exported
           #:abstract-p
           #:cache-p
           #:prefetch-p
           #:index-p
           #:unique-p
           #:required-p
           #:association-end-query-of
           #:direct-instances-identity-view-of
           #:direct-instances-data-view-of
           #:all-instances-identity-view-of
           #:all-instances-data-view-of
           #:-instance-
           #:-associated-instance-

           ;; defining persistent associations
           #:defpassociation
           #:defpassociation*
           #:find-persistent-association
           #:to-one-association-end-p
           #:to-many-association-end-p

           ;; defining persistent types
           #:find-type
           #:normalized-type-of
           #:parse-type
           #:unparse-type
           #:primitive-type-p
           #:primitive-type-p*
           #:persistent-class-type-p
           #:persistent-class-type-p*
           #:destructure-type
           #:def-member-type

           ;; persistent class meta objects
           #:persistent-class
           #:persistent-association
           #:persistent-slot-definition
           #:persistent-direct-slot-definition
           #:persistent-effective-slot-definition
           #:persistent-association-end-slot-definition
           #:persistent-association-end-direct-slot-definition
           #:persistent-association-end-effective-slot-definition
           #:persistent-type
           #:integer-type
           #:float-type
           #:string-type
           #:text-type
           #:member-type
           #:serialized-type
           #:symbol-type

           ;; primitve types
           #:t
           #:serialized
           #:unbound
           #:null
           #:boolean
           #:integer-8
           #:integer-16
           #:integer-32
           #:integer-64
           #:integer
           #:float-32
           #:float-64
           #:float
           #:double
           #:double-float
           #:number
           #:string
           #:text
           #:symbol
           #:symbol*
           #:time
           #:timestamp
           #:duration
           #:form
           #:unsigned-byte-vector
           #:member
           #:set
           #:disjunct-set
           #:ordered-set
           #:or

           ;; persistent classes, all persistent-classes are valid slot types
           #:persistent-object
           #:persistent-set
           #:persistent-set-element

           ;; most of the transaction API is inherited from hu.dwim.rdbms
           #:with-database
           #:*database*
           #:with-transaction
           #:with-transaction*
           #:with-readonly-transaction
           #:in-transaction-p
           #:call-in-transaction
           #:begin-transaction
           #:commit-transaction
           #:rollback-transaction
           #:*transaction*
           #:register-transaction-hook
           #:mark-transaction-for-commit-only
           #:mark-transaction-for-rollback-only
           #:transaction-timestamp
           #:transaction-mixin
           #:transaction-with-hooks-mixin
           #:before-committing-instance
           #:after-instance-committed

           ;; managing persistent instances
           #:with-making-persistent-instances
           #:with-making-transient-instances
           #:ensure-persistent
           #:ensure-transient
           #:make-persistent
           #:make-persistent-using-class
           #:make-transient
           #:make-transient-using-class
           #:lock-class
           #:lock-instance
           #:lock-slot
           #:oid-of
           #:persistent-p
           #:created-p
           #:modified-p
           #:deleted-p
           #:instance-in-transaction-p
           #:instance-in-current-transaction-p
           #:transaction-of
           #:p-eq
           #:load-instance
           #:revive-instance
           #:revive-instances
           #:with-revived-instance
           #:with-revived-instances
           #:with-reloaded-instance
           #:with-reloaded-instances
           #:purge-instance
           #:purge-instances
           #:count-instances
           #:print-persistent-instance
           #:copy-persistent-instance
           #:copy-into-transaction-cache
           #:unbound-slot-marker-p

           ;; most of the collection API is inherited from cl-containers
           #:insert-item
           #:ensure-item
           #:first-item
           #:delete-item
           #:size
           #:empty-p
           #:empty!
           #:find-item
           #:iterate-items
           #:list-of
           #:items-of
           #:sets-of

           ;; consistency checks
           #:signal-broken-database
           #:signal-broken-references
           #:signal-broken-instances

           ;; query
           #:make-query-for-classes-and-slots
           #:query
           #:copy-query
           #:query-builder
           #:simple-query-builder
           #:select
           #:simple-select
           #:purge
           #:update
           #:from
           #:where
           #:group-by
           #:having
           #:order-by
           #:offset
           #:limit
           #:select-first-matching-instance
           #:select-last-matching-instance
           #:select-instance
           #:select-instances
           #:select-similar-instance
           #:select-similar-instances
           #:select-the-only-one
           #:make-query
           #:current-query-variable-of
           #:add-lexical-variable
           #:add-query-variable
           #:add-assert
           #:add-order-by
           #:add-collect
           #:execute-query
           #:unique
           #:flat
           #:result-set
           #:result-type-of
           #:define-query-macro
           #:like
           #:re-like
           #:avg
           #:sum
           #:volatile
           #:static
           #:sql-text

           ;; scroll
           #:scroll
           #:elements
           #:page
           #:first-page!
           #:previous-page!
           #:next-page!
           #:last-page!
           #:page-count
           #:page-size
           #:element-count
           #:fixed-size-scroll
           #:simple-scroll

           ;; dimensional
           #:persistent-class-d
           #:persistent-class-h
           #:persistent-association-d
           #:persistent-slot-definition-d
           #:persistent-direct-slot-definition-d
           #:persistent-effective-slot-definition-d
           #:persistent-association-end-slot-definition-d
           #:persistent-association-end-direct-slot-definition-d
           #:persistent-association-end-effective-slot-definition-d
           #:coordinate
           #:&coordinate
           #:coordinate-begin
           #:coordinate-end
           #:dimension
           #:ordering-dimension
           #:inheriting-dimension
           #:whole-domain-marker-p
           #:h-unused-slot-marker-p
           #:time
           #:with-coordinate
           #:with-coordinates
           #:call-with-coordinate
           #:call-with-coordinates
           #:with-time
           #:with-default-time
           #:validity
           #:with-validity
           #:with-validity-range
           #:*time-begin*
           #:*time-end*
           #:*validity-begin*
           #:*validity-end*
           #:d-value
           #:d-value-p
           #:make-single-d-value
           #:single-d-value-p
           #:d-value=
           #:time-of
           #:validity-begin-of
           #:validity-end-of
           #:+beginning-of-time+
           #:+end-of-time+
           #:persistent-object-d
           #:persistent-object-h
           #:time-dependent-object
           #:validity-dependent-object
           #:d-instance-of
           #:h-instances
           #:h-instances-of
           #:integrated-time-dependent-slot-value
           #:averaged-time-dependent-slot-value

           ;; turning cache on and off
           #:with-caching-slot-values
           #:without-caching-slot-values

           ;; laziness
           #:with-lazy-slot-value-collections
           #:without-lazy-slot-value-collections

           ;; type check
           #:with-type-checking-slot-values
           #:without-type-checking-slot-values

           ;; update optimization
           #:with-storing-equal-slot-values
           #:without-storing-equal-slot-values

           ;; export,import
           #:write-persistent-object-by-oid
           #:read-persistent-object-by-oid)

  (:readtable-setup
   (hu.dwim.util:enable-standard-hu.dwim-syntaxes)
   (hu.dwim.syntax-sugar:enable-lambda-with-bang-args-syntax)))

(in-package :hu.dwim.perec)

(import-sql-syntax-node-names)

(import-sql-constructor-names)

(def computed-universe computed-universe/perec ()
  ()
  (:computed-state-factory-name compute-as))

(dolist (node-class (hu.dwim.walker:collect-standard-walked-form-subclasses))
  (bind ((node-class-name (class-name node-class)))
    (shadow node-class-name :hu.dwim.perec)
    (intern (symbol-name node-class-name) :hu.dwim.perec)))
