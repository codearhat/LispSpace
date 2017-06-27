;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms.sqlite)

(def method format-sql-syntax-node ((type sql-integer-type) (database sqlite))
  (format-string "INTEGER"))

(def method format-sql-syntax-node ((type sql-float-type) (database sqlite))
  (format-string "REAL"))

(def method format-sql-syntax-node ((type sql-character-large-object-type) (database sqlite))
  (format-string "TEXT"))

(def method format-sql-syntax-node ((type sql-binary-large-object-type) (database sqlite))
  (format-string "BLOB"))
