;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms)

(def syntax-node sql-create-sequence (sql-ddl-statement)
  ((name
    :type string)
   (temporary
    #f
    :type boolean))
  (:documentation "An SQL CREATE SEQUENCE statement.")
  (:format-sql-syntax-node
   (format-string "CREATE")
   (when temporary
     (format-string " TEMPORARY"))
   (format-string " SEQUENCE ")
   (format-sql-identifier name)))

(def syntax-node sql-drop-sequence (sql-ddl-statement)
  ((name
    :type string)
   (ignore-missing
    #f
    :type boolean))
  (:documentation "An SQL DROP SEQUENCE statement.")
  (:format-sql-syntax-node
   (format-string "DROP")
   (format-string " SEQUENCE ")
   (when ignore-missing
     (format-string "IF EXISTS "))
   (format-sql-identifier name)))

(def syntax-node sql-sequence-nextval-column (sql-syntax-node)
  ((name
    :type string))
  (:documentation "An SQL SEQUENCE next value column.")
  (:format-sql-syntax-node
   (format-string "NEXTVAL('")
   (format-sql-identifier name)
   (format-string "')")))

(def syntax-node sql-sequence-currval-column (sql-syntax-node)
  ((name
    :type string))
  (:documentation "An SQL SEQUENCE current value column.")
  (:format-sql-syntax-node
   (format-string "CURRVAL('")
   (format-sql-identifier name)
   (format-string "')")))

(def syntax-node sql-sequence-setval (sql-syntax-node)
  ((name
    :type string)
   (value
    :type integer)
   (currentp
    :type boolean))
  (:documentation "SQL SEQUENCE setter.")
  (:format-sql-syntax-node
   (format-string "SETVAL('")
   (format-sql-identifier name)
   (format-string "',")
   (format-sql-literal value)
   (format-string ",")
   (format-sql-literal currentp)
   (format-string ")")))
