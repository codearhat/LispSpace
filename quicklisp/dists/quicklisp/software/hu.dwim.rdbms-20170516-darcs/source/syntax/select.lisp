;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms)

;; TODO: rename columns and tables to something more general?
(def syntax-node sql-select (sql-dml-statement)
  ((distinct
    nil
    :type boolean)
   (columns
    :type (list sql-column-alias*))
   (tables
    nil
    :type (list sql-table-reference))
   (where
    nil
    :type sql-expression)
   (group-by
    nil
    :type (list sql-expression))
   (having
    nil
    :type sql-expression)
   (order-by
    nil
    :type list)                         ; TODO: element type
   (offset
    nil
    :type (or null integer))
   (limit
    nil
    :type (or null integer))
   (for
    nil
    :type (member :update :share)
    :accessor for-of)
   (wait
    #t
    :type boolean))
  (:documentation "An SQL SELECT statement.")
  (:format-sql-syntax-node
   (format-string "SELECT ")
   (when distinct
     (format-string "DISTINCT "))
   (format-comma-separated-identifiers columns)
   (when tables
     (format-string " FROM ")
     (format-comma-separated-list tables 'format-sql-table-reference))
   (format-sql-where where)
   (when group-by
     (format-string " GROUP BY ")
     (format-comma-separated-list group-by))
   (when having
     (format-string " HAVING ")
     (format-sql-syntax-node having))
   (when order-by
     (format-string " ORDER BY ")
     (format-comma-separated-list order-by))
   (when limit
     (format-string " LIMIT ")
     (format-sql-syntax-node limit))
   (when offset
     (format-string " OFFSET ")
     (format-sql-syntax-node offset))
   (when for
     (format-string " FOR ")
     (format-string (symbol-name for))
     (unless wait
       (format-string " NOWAIT")))))

;; TODO shouldn't the NAME slot be called TABLE and be typed (or sql-identifier* sql-table)?
(def syntax-node sql-table-alias (sql-identifier)
  ((name
    :type sql-identifier*)
   (alias
    nil
    :type sql-identifier*))
  (:format-sql-identifier
   (format-sql-identifier name)
   (when alias
     (format-string " ")
     (format-sql-identifier alias))))

(def type sql-table-alias* ()
  '(or string symbol sql-identifier sql-table-alias))

(def syntax-node sql-derived-table (sql-syntax-node)
  ((subquery
    :type )
   (alias
    nil
    :type sql-identifier*))
  (:format-sql-syntax-node
   (format-sql-syntax-node subquery)
   (when alias
     (format-char " ")
     (format-sql-identifier alias))))

(def syntax-node sql-joined-table (sql-syntax-node)
  ((kind
    :type (member :cross :inner :left :right :full :union))
   (left
    :type (or sql-table-reference list))
   (right
    :type (or sql-table-reference list))
   (on
    nil
    :type sql-expression)
   (using
    nil
    :type (list sql-identifier*)))
  (:format-sql-syntax-node
   (format-char "(")
   (if (atom left)
       (format-sql-syntax-node left)
       (format-comma-separated-list left))
   (format-char " ")
   (format-string (string-upcase kind))
   (format-string " JOIN ")
   (if (atom right)
       (format-sql-syntax-node right)
       (format-comma-separated-list right))
   (when on
     (format-string " ON ")
     (format-sql-syntax-node on))
   (when using
     (format-string " USING ")
     (format-char "(")
     (format-comma-separated-identifiers using)
     (format-char ")"))
   (format-char ")")))

(def type sql-table-reference ()
  '(or sql-table-alias* sql-derived-table sql-joined-table))

(def function format-sql-table-reference (reference database)
  (etypecase reference
    (sql-table-alias* (format-sql-identifier reference database))
    ((or sql-derived-table sql-joined-table sql-unquote) (format-sql-syntax-node reference database))))

(def syntax-node sql-column-alias (sql-identifier)
  ((table
    nil
    :type sql-identifier*)
   (column
    :type sql-identifier*)
   (alias
    nil
    :type sql-identifier*))
  (:format-sql-identifier
   (awhen table
     (format-sql-identifier table)
     (format-char "."))
   (format-sql-identifier column)
   (when alias
     (format-string " AS ")
     (format-sql-identifier alias))))

(def type sql-column-alias* ()
  '(or string symbol sql-column-alias))

(def syntax-node sql-all-columns (sql-identifier)
  ()
  (:format-sql-identifier
   (format-char "*")))

(def syntax-node sql-distinct-spec (sql-identifier)
  ((column-spec
    :type (or integer sql-column-alias*)))
  (:format-sql-identifier
   (format-string "DISTINCT ")
   (format-sql-syntax-node column-spec)))

(def syntax-node sql-sort-spec (sql-syntax-node)
  ((sort-key
    :type (or integer sql-column-alias*))
   (ordering
    :ascending
    :type (member :ascending :descending)))
  (:format-sql-syntax-node
   (format-sql-syntax-node sort-key)
   (format-char " ")
   (ecase ordering
     (:ascending (format-string "ASC"))
     (:descending (format-string "DESC")))))
