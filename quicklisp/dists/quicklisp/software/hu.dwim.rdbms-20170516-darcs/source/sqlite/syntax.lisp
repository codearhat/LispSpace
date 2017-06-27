;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms.sqlite)

(def method format-sql-syntax-node ((-self- sql-create-table) (database sqlite))
  (with-slots (temporary name columns as) -self-
    (format-string "CREATE")
    (when temporary
      (format-string " TEMPORARY"))
    (format-string " TABLE ")
    (format-sql-identifier name database)
    (format-string " (")
    (format-comma-separated-list columns database)
    (format-char ")")
    (when (and temporary (not (eq temporary #t)) (not as))
      (format-string " ON COMMIT ")
      (format-string (ecase temporary
                       (:drop "DROP")
                       (:preserve-rows "PRESERVE ROWS")
                       (:delete-rows "DELETE ROWS"))))
    (when as
      (format-string " AS ")
      (format-sql-syntax-node as database))))

;;;;;;
;;; Literals

(def method format-sql-literal ((value (eql nil)) (database sqlite))
  (format-string "0"))

(def method format-sql-literal ((value (eql t)) (database sqlite))
  (format-string "1"))

(def method format-sql-literal ((literal sql-literal) (database sqlite))
  (if (unquote-aware-format-sql-literal literal)
      (progn
        (format-string ":")
        (format-string (princ-to-string (length *binding-types*))))
      (call-next-method)))

;;;;;;
;;; Bindings

(def method format-sql-syntax-node ((variable sql-binding-variable) (database sqlite))
  (unquote-aware-format-sql-binding-variable variable)
  (format-string ":")
  (format-string (princ-to-string (length *binding-types*))))
