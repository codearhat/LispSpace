;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms)

(def syntax-node sql-update (sql-dml-statement)
  ((table
       :type sql-identifier*)
   (columns
    :type (list sql-identifier*))
   (values
    :type (list sql-literal*)
    :accessor values-of)
   (where
    nil
    :type sql-expression))
  (:documentation "An SQL UPDATE statement.")
  (:format-sql-syntax-node
   (format-string "UPDATE ")
   (format-sql-identifier table)
   (format-string " SET ")
   (iter (for column in columns)
         (for value in values)
         (unless (first-iteration-p)
           (format-string ", "))
         (format-sql-identifier column)
         (format-string " = ")
         (format-sql-syntax-node value))
   (format-sql-where where)))
