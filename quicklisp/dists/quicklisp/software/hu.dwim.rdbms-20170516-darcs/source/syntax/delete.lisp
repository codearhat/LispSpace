;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms)

(def syntax-node sql-delete (sql-dml-statement)
  ((table
    :type sql-identifier*)
   (where
    nil
    :type (or null sql-expression)))
  (:documentation "An SQL DELETE statement.")
  (:format-sql-syntax-node
   (format-string "DELETE FROM ")
   (format-sql-identifier table)
   (format-sql-where where)))
