;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms)

(def syntax-node sql-create-table (sql-ddl-statement)
  ((name
    :type sql-identifier*)
   (temporary
    nil
    :type (or boolean (member (:drop :preserve-rows :delete-rows))))
   (columns
    nil
    :type list)
   (as
    nil
    :type (or sql-select sql-set-operation-expression)))
  (:documentation "An SQL CREATE TABLE statement.")
  (:format-sql-syntax-node
   (format-string "CREATE")
   (when temporary
     (format-string " GLOBAL TEMPORARY"))
   (format-string " TABLE ")
   (format-sql-identifier name)
   (format-string " (")
   (format-comma-separated-list columns)
   (format-char ")")
   (when (and temporary (not (eq temporary #t)) (not as))
     (format-string " ON COMMIT ")
     (format-string (ecase temporary
                      (:drop "DROP")
                      (:preserve-rows "PRESERVE ROWS")
                      (:delete-rows "DELETE ROWS"))))
   (when as
     (format-string " AS ")
     (format-sql-syntax-node as))))

(def syntax-node sql-create-view (sql-create-table)
  ((replace
    #f
    :type boolean
    :accessor replace-p))
  (:documentation "An SQL CREATE TABLE statement.")
  (:format-sql-syntax-node
   (format-string "CREATE")
   (when replace
     (format-string " OR REPLACE"))
   (when temporary
     (format-string " TEMPORARY"))
   (format-string " VIEW ")
   (format-sql-identifier name)
   (awhen columns
     (format-string " (")
     (format-comma-separated-identifiers columns)
     (format-char ")"))
   (format-string " AS ")
   (format-sql-syntax-node as)))

(def syntax-node sql-column (named-sql-syntax-node)
  ((type
    :type sql-type)
   (constraints
    '()
    :type list)
   (default-value
    :type sql-syntax-node
    :documentation "May be an sql literal or an sql expression AST node. Note: expressions as default values are not supported on all backends."))
  (:documentation "An SQL column specification.")
  (:format-sql-syntax-node
   (format-sql-identifier name)
   (when type
     (format-char " ")
     (format-sql-syntax-node type))
   (when (slot-boundp -self- 'default-value)
     (format-string " DEFAULT (")
     (format-sql-syntax-node default-value)
     (format-string ")"))
   (mapc (lambda (constraint)
	   (format-sql-syntax-node constraint))
	 constraints))
  (:format-sql-identifier
   (format-sql-identifier name)))
