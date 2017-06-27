;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms)

(def syntax-node sql-drop-table (sql-ddl-statement)
  ((name
    :type string)
   (ignore-missing
    #f
    :type boolean)
   (cascade
    #f
    :type boolean))
  (:documentation "An SQL DROP TABLE statement.")
  (:format-sql-syntax-node
    (format-string "DROP TABLE ")
    (when ignore-missing
      (format-string "IF EXISTS "))
    (format-sql-identifier name)
    (when cascade
      (format-string " CASCADE"))))

(def syntax-node sql-drop-view (sql-ddl-statement)
  ((name
    :type string)
   (ignore-missing
    #f
    :type boolean))
  (:documentation "An SQL DROP VIEW  statement.")
  (:format-sql-syntax-node
    (format-string "DROP VIEW ")
    (when ignore-missing
      (format-string "IF EXISTS "))
    (format-sql-identifier name)))
