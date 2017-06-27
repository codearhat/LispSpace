;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :hu.dwim.rdbms
  (:use :babel
        :hu.dwim.asdf
        :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.logger
        :hu.dwim.syntax-sugar
        :hu.dwim.util)

  (:shadow #:type-of
           #:as)

  (:export #:notify-transaction-event
           #:in-transaction-p
           #:transaction-in-progress-p
           #:transaction-valid-p
           #:register-transaction-hook
           #:transaction-with-hooks-mixin
           #:transaction-timestamp
           #:rdbms-name-for

           #:table-exists-p
           #:unbound-binding-variable-error

           #:create-view
           #:drop-view
           #:update-view
           #:list-views
           #:view-exists-p

           #:create-sequence
           #:drop-sequence
           #:sequence-exists-p
           #:sequence-next

           #:sql
           #:format-sql
           #:format-sql-to-string
           #:import-sql-syntax-node-names
           #:import-sql-constructor-names

           #:sql-cond
           #:sql-if

           #:mark-transaction-for-commit-only
           #:mark-transaction-for-rollback-only

           #:insert-record
           #:update-records
           #:delete-records
           #:select-records
           #:select-count-*

           #:safepoint
           #:release-safepoint
           #:rollback-to-safepoint

           #:make-cursor
           #:cursor-position
           #:column-count
           #:row-count
           #:column-name
           #:column-type
           #:column-value
           #:for-each-row
           #:collect-rows
           #:current-row

           #:sql-create-table
           #:sql-create-index
           #:sql-create-view
           #:sql-foreign-key-constraint
           #:sql-add-foreign-key-constraint-action
           #:constraints-of
           #:constraint-to-action
           #:list-table-foreign-keys
           #:database-list-table-foreign-keys
           #:sql-drop-constraint-action
           #:foreign-key-descriptor
           #:drop-foreign-key
           #:drop-table-foreign-keys
           #:*signal-non-destructive-alter-table-commands*
	   #:oid-default-statement-of)
  (:readtable-setup
   (hu.dwim.util:enable-standard-hu.dwim-syntaxes)
   (hu.dwim.syntax-sugar:enable-lambda-with-bang-args-syntax)))

(in-package :local-time)

;; KLUDGE TODO oh, god, please FIXME when local-time gets date/time support
(defun parse-datestring (string)
  (let* ((*default-timezone* +utc-zone+)
         (date (parse-timestring string :offset 0)))
    (unless (and date
                 (zerop (sec-of date))
                 (zerop (nsec-of date)))
      (error "~S is not a valid date string" string))
    date))

(export 'parse-datestring)
