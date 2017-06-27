;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms)

(def (condition* e) rdbms-error ()
  ())

(def condition* translated-rdbms-error (rdbms-error)
  ((original-error)))

(def (condition* e) simple-rdbms-error (simple-error rdbms-error)
  ())

(def function simple-rdbms-error (message &rest format-arguments)
  (error 'simple-rdbms-error :format-control message :format-arguments format-arguments))

(def (condition* e) unable-to-obtain-lock-error (translated-rdbms-error simple-rdbms-error)
  ())

(def (condition* e) deadlock-detected-error (translated-rdbms-error simple-rdbms-error)
  ())

(def function %signal-translated-simple-rdbms-error (type message-or-nested-condition)
  (error type
         :format-control (princ-to-string message-or-nested-condition)
         :original-error (when (typep message-or-nested-condition 'condition)
                           message-or-nested-condition)))

(def function deadlock-detected-error (message-or-nested-condition)
  (%signal-translated-simple-rdbms-error 'deadlock-detected-error message-or-nested-condition))

(def function unable-to-obtain-lock-error (message-or-nested-condition)
  (%signal-translated-simple-rdbms-error 'unable-to-obtain-lock-error message-or-nested-condition))

(def condition* transaction-error (simple-rdbms-error)
  ())

;;;;;;
;;; DDL

(def (condition* e) unconfirmed-schema-change (serious-condition)
  ((table-name
    :type string)
   (column-name
    :type string)))

(def (condition* e) unconfirmed-schema-change/add-column (unconfirmed-schema-change)
  ((column-type))
  (:report (lambda (error stream)
             (format stream "Adding the column ~S with type ~A in table ~S is a safe operation"
                     (column-name-of error) (column-type-of error) (table-name-of error)))))

(def (condition* e) unconfirmed-schema-change/create-table (unconfirmed-schema-change)
  ()
  (:report (lambda (error stream)
             (format stream "Adding the table ~S is a safe operation" (table-name-of error)))))

(def (condition* e) unconfirmed-schema-change/add-foreign-key (unconfirmed-schema-change)
  ((constraint-name))
  (:report (lambda (error stream)
             (format stream "Adding a foreign key ~A is a safe operation"
                     (constraint-name-of error)))))

(def (condition* e) unconfirmed-schema-change/drop-foreign-key (unconfirmed-schema-change)
  ((constraint-name))
  (:report (lambda (error stream)
             (format stream "Dropping a foreign key ~A is a safe operation"
                     (constraint-name-of error)))))

(def (condition* e) unconfirmed-schema-change/replace-foreign-key (unconfirmed-schema-change)
  ((constraint-name))
  (:report (lambda (error stream)
             (format stream "Replacing a foreign key ~A is a safe operation"
                     (constraint-name-of error)))))

;;;;;;
;;; destructive DDL

(def (condition* e) unconfirmed-destructive-schema-change (unconfirmed-schema-change)
  ())

(def (condition* e) unconfirmed-destructive-schema-change/alter-column-type (unconfirmed-destructive-schema-change)
  ((old-type)
   (new-type)
   (new-rdbms-type))
  (:report (lambda (error stream)
             (format stream "Changing the type of column ~S from the current rdbms type ~A to new rdbms type ~A (derived from ~A) in table ~S will be issued in a separate transaction and your database will try to convert existing data. THIS MAY RESULT IN DATA LOSS!"
                     (column-name-of error) (old-type-of error) (new-rdbms-type-of error) (new-type-of error) (table-name-of error)))))

(def (condition* e) unconfirmed-destructive-schema-change/drop-column (unconfirmed-destructive-schema-change)
  ()
  (:report (lambda (error stream)
             (format stream "Dropping the column ~S from table ~S is a destructive operation, which needs confirmation"
                     (column-name-of error) (table-name-of error)))))
