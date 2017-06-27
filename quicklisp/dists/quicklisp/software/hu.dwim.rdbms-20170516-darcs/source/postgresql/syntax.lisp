;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms.postgresql)

(def method format-sql-syntax-node ((action sql-add-column-action) (database postgresql))
  (format-string "ADD ")
  (format-sql-identifier (name-of action) database)
  (format-char " ")
  (format-sql-syntax-node (type-of action) database))

(def method format-sql-literal ((literal vector) (database postgresql))
  (format-string "E'")
  (loop for el across literal
        do (if (or (<= 0 el 31)
                   (<= 127 el 255)
                   (= el #.(char-code #\'))
                   (= el #.(char-code #\\)))
               (format *sql-stream* "\\\\~3,'0o" el)
               (format-char (code-char el))))
  (format-string "'::bytea"))

(def method format-sql-literal ((literal sql-literal) (database postgresql))
  (if (unquote-aware-format-sql-literal literal)
      (progn
        (format-string "$")
        (format-string (princ-to-string (length *binding-types*)))
        (format-string "::")
        (format-sql-syntax-node (type-of literal) database))
      (call-next-method)))

(def method format-sql-syntax-node ((variable sql-binding-variable) (database postgresql))
  (unquote-aware-format-sql-binding-variable variable)
  (format-string "$")
  (format-string (princ-to-string (length *binding-types*)))
  (awhen (type-of variable)
    (format-string "::")
    (format-sql-syntax-node (type-of variable) database)))

(def method format-sql-syntax-node ((like sql-like) (database postgresql))
  (with-slots (string pattern case-sensitive-p) like
    (format-char "(")
    (format-sql-syntax-node string database)
    (if case-sensitive-p
        (format-string " LIKE ")
        (format-string " ILIKE "))
    (format-sql-syntax-node pattern database)
    (format-char ")")))

(def method format-sql-syntax-node ((regexp-like sql-regexp-like) (database postgresql))
  (format-char "(")
  (format-sql-syntax-node (string-of regexp-like) database)
  (format-char " ")
  (format-string (if (case-sensitive-p regexp-like) "~" "~*"))
  (format-char " ")
  (format-sql-syntax-node (pattern-of regexp-like) database)
  (format-char ")"))

(def method equal-type-p ((type-1 sql-binary-large-object-type) (type-2 sql-binary-large-object-type) (database postgresql))
  ;; don't compare size, because postgresql has no fixed size binary, so it can't be extracted from the schema
  (eq (class-of type-1) (class-of type-2)))

(def method equal-type-p ((type-1 sql-integer-type) (type-2 sql-numeric-type) (database postgresql))
  (not (bit-size-of type-1)))

(def method equal-type-p ((type-1 sql-numeric-type) (type-2 sql-integer-type) (database postgresql))
  (not (bit-size-of type-2)))
