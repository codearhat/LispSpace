;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;;;
;;; RDBMS model classes

;; TODO: use sql-table when available from hu.dwim.rdbms
(def computed-class* table (exportable)
  ((name
    :type string
    :documentation "The name of the RDBMS table.")
   (columns
    (compute-as nil)
    :type (list sql-column)
    :documentation "The list of RDBMS columns of this table. This list uses the sql column type of hu.dwim.rdbms."))
  (:documentation "An RDBMS table with some related RDBMS definitions. The actual table will be created in the database when export-to-rdbms is called on it."))

(def computed-class* view (exportable)
  ((name
    :type string
    :documentation "The name of the RDBMS view.")
   (columns
    :type list
    :documentation "The column names of the view.")
   (query
    :type sql-query-expression
    :documentation "The SQL create view statement.")))

(def computed-class* column (sql-column)
  ((index
    (compute-as nil)
    :type (or null sql-index)
    :documentation "An RDBMS index on this column."))
  (:documentation "An RDBMS column with some related RDBMS specific definitions."))

(def print-object table
  (princ (name-of -self-)))

(def print-object column
  (princ (hu.dwim.rdbms::name-of -self-)))

;;;;;;
;;; Export

(def method export-to-rdbms ((table table))
  "Updates the RDBMS table definition according to the current state of the given table. This might add, alter or drop existing columns, but all destructive changes are required to signal a continuable condition."
  (update-table (name-of table) (columns-of table))
  (mapc [awhen (index-of !1)
          (update-index (hu.dwim.rdbms::name-of it) (name-of table) (list !1) :unique (hu.dwim.rdbms::unique-p it))]
        (columns-of table)))

(def method export-to-rdbms ((view view))
  (update-view (name-of view) (columns-of view) (query-of view)))

;;;;;;
;;; Functional

(def function column-equal-p (column-1 column-2)
  (and (equalp (hu.dwim.rdbms::name-of column-1)
               (hu.dwim.rdbms::name-of column-2))
       (hu.dwim.rdbms::equal-type-p (hu.dwim.rdbms::type-of column-1)
                                    (hu.dwim.rdbms::type-of column-2)
                                    *database*)
       (or (and (not (slot-boundp column-1 'hu.dwim.rdbms::default-value))
                (not (slot-boundp column-2 'hu.dwim.rdbms::default-value)))
           (equalp (hu.dwim.rdbms::default-value-of column-1)
                   (hu.dwim.rdbms::default-value-of column-2)))
       (every [eq (class-of !1) (class-of !2)]
              (hu.dwim.rdbms::constraints-of column-1)
              (hu.dwim.rdbms::constraints-of column-2))))
