;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms)

(def special-variable *sql-operator-names* (list)
  "A list of symbols that name an SQL operator")

(def special-variable *sql-function-names* (list)
  "A list of symbols that name an SQL function")

;;;;;;
;;; Expressions

(def syntax-node sql-expression (sql-syntax-node)
  ())

(def syntax-node sql-query-expression (sql-expression sql-dml-statement)
  ())

;;;;;;
;;; Set operations

(def syntax-node sql-set-operation-expression (sql-query-expression)
  ((set-operation
    :type (member :union :except :intersect))
   (all
    #f
    :type boolean)
   (subqueries
    nil
    :type (list sql-query-expression)))
  (:format-sql-syntax-node
   (format-char "(")
   (format-separated-list subqueries
                          (string+ (symbol-name set-operation) (if all " ALL" "")))
   (format-char ")")))

(def definer set-operation (name)
  (let ((constructor-name (sql-constructor-name name)))
    `(progn
      (pushnew ',constructor-name *sql-constructor-names*)
      (pushnew ',name *sql-operator-names*)
      (def function ,constructor-name (&rest subqueries)
        (make-instance 'sql-set-operation-expression
                       :set-operation ,(intern (symbol-name name) (find-package :keyword))
                       :subqueries subqueries)))))

(def set-operation union)

(def set-operation except)

(def set-operation intersect)

;;;;;;
;;; Operators

(def syntax-node sql-operator (sql-expression named-sql-syntax-node)
  ())

(def syntax-node sql-unary-operator (sql-operator)
  ((fix
    :prefix
    :type (member :prefix :postfix))
   (expression
    :type sql-expression))
  (:format-sql-syntax-node
   (format-char "(")
   (ecase fix
     (:prefix
      (format-sql-operator-name name database)
      (format-char " ")
      (format-sql-syntax-node expression))
     (:postfix
      (format-sql-syntax-node expression)
      (format-char " ")
      (format-sql-operator-name name database)))
   (format-char ")")))

(def syntax-node sql-binary-operator (sql-operator)
  ((left
    :type sql-expression)
   (right
    :type sql-expression))
  (:format-sql-syntax-node
   (format-char "(")
   (format-sql-syntax-node left)
   (format-char " ")
   (format-sql-operator-name name database)
   (format-char " ")
   (format-sql-syntax-node right)
   (format-char ")")))

(def syntax-node sql-n-ary-operator (sql-operator)
  ((expressions
    :type list))
  (:format-sql-syntax-node
   (format-char "(")
   (format-separated-list expressions name)
   (format-char ")")))

(def definer unary-operator (name &optional (fix :prefix))
  (let ((constructor-name (sql-constructor-name name)))
    `(progn
      (pushnew ',constructor-name *sql-constructor-names*)
      (pushnew ',name *sql-operator-names*)
      (def function ,constructor-name (expression)
        (make-instance 'sql-unary-operator
                       :name ,(sql-operator-name name)
                       :fix ,fix
                       :expression expression)))))

(def definer binary-operator (name)
  (let ((constructor-name (sql-constructor-name name)))
    `(progn
      (pushnew ',constructor-name *sql-constructor-names*)
      (pushnew ',name *sql-operator-names*)
      (def function ,constructor-name (left right)
        (make-instance 'sql-binary-operator
                       :name ,(string-upcase name)
                       :left left
                       :right right)))))

(def definer n-ary-operator (name)
  (let ((constructor-name (sql-constructor-name name)))
    `(progn
      (pushnew ',constructor-name *sql-constructor-names*)
      (pushnew ',name *sql-operator-names*)
      (def function ,constructor-name (&rest expressions)
        (make-instance 'sql-n-ary-operator
                       :name ,(string-upcase name)
                       :expressions expressions)))))

(def definer varary-operator (name)
  (let ((constructor-name (sql-constructor-name name)))
    `(progn
      (pushnew ',constructor-name *sql-constructor-names*)
      (pushnew ',name *sql-operator-names*)
      (def function ,constructor-name (&rest expressions)
        (if (length= 1 expressions)
            (make-instance 'sql-unary-operator
                           :name ,(string-upcase name)
                           :expression (first expressions))
            (make-instance 'sql-n-ary-operator
                           :name ,(string-upcase name)
                           :expressions expressions))))))

;;;;;;
;;; Logical operators

(def unary-operator not)

(def n-ary-operator and)

(def n-ary-operator or)

;;;;;;
;;; Set operators

(def binary-operator in)

;;;;;;
;;; Comparison operators

(def binary-operator =)

(def binary-operator <)

(def binary-operator >)

(def binary-operator <=)

(def binary-operator >=)

(def binary-operator <>)

(def unary-operator is-null :postfix)

(def unary-operator is-not-null :postfix)

;;;;;;
;;; Arithmetic operators

(def varary-operator +)

(def varary-operator -)

(def n-ary-operator *)

(def binary-operator /)

(def binary-operator %)

(def binary-operator ^)

(def n-ary-operator \|\|)

(def unary-operator \|/)

(def unary-operator @)

;;;;;;
;;; Bitwise operators

(def binary-operator &)

(def binary-operator \|)

(def binary-operator \#)

(def unary-operator ~)

(def binary-operator |<<|)

(def binary-operator |>>|)

;;;;;;
;;; Pattern matching

(def syntax-node sql-like (sql-expression)
  ((string :type  (or sql-expression sql-column-alias) :accessor string-of)
   (pattern :type (or sql-expression sql-literal))
   (case-sensitive-p #t :type boolean))
  (:format-sql-syntax-node
   (if case-sensitive-p
       (progn
         (format-char "(")
         (format-sql-syntax-node string)
         (format-string " LIKE ")
         (format-sql-syntax-node pattern)
         (format-char ")"))
       (progn
         (format-string "(UPPER(")
         (format-sql-syntax-node string)
         (format-string ") LIKE UPPER(")
         (format-sql-syntax-node pattern)
         (format-string "))")))))

(def syntax-node sql-regexp-like (sql-expression)
  ((string :type sql-expression :accessor string-of)
   (pattern :type sql-expression)
   (case-sensitive-p #t :type boolean)))

;;;;;;
;;; Case expressions

(def syntax-node sql-case (sql-expression)
  ((clauses :type list))
  (:format-sql-syntax-node
   (format-char "(")
   (format-string "CASE")
   (dolist (clause clauses)
     (let ((when (first clause))
           (then (second clause)))
       (format-char " ")
       (if (eq when t)
           (format-string "ELSE")
           (progn
             (format-string "WHEN")
             (format-char " ")
             (format-sql-syntax-node when)
             (format-char " ")
             (format-string "THEN")))
       (format-char " ")
       (format-sql-syntax-node then)))
   (format-char " ")
   (format-string "END")
   (format-char ")")))

(def function sql-cond (clauses)
  (sql-case :clauses clauses))

(def function sql-if (cond then else)
  (sql-case :clauses `((,cond ,then) (t ,else))))

;;;;;;
;;; Subquery expressions

(def syntax-node sql-subquery (sql-query-expression)
  ((query
    :type sql-select)) ;; TODO: extract query-expression from the ddl statement
  (:format-sql-syntax-node
   (format-char "(")
   (format-sql-syntax-node query)
   (format-char ")")))

(def unary-operator exists)

;;;;;;
;;; Functions

(def syntax-node sql-function-call (sql-expression)
  ((name
    :type (or string symbol))
   (arguments
    nil))
  (:format-sql-syntax-node
   (format-sql-function-name name database)
   (format-char "(")
   (format-comma-separated-list arguments)
   (format-char ")")))

(def definer aggregate-function (name)
  (let ((constructor-name (sql-constructor-name name)))
    `(progn
      (pushnew ',constructor-name *sql-constructor-names*)
      (pushnew ',name *sql-function-names*)
      (def function ,constructor-name (&rest arguments)
        (make-instance 'sql-function-call
                       :name ,(string-upcase name)
                       :arguments arguments)))))

;;;;;;
;;; Aggregate functions

(def aggregate-function count)

(def aggregate-function distinct)

(def aggregate-function min)

(def aggregate-function max)

(def aggregate-function avg)

(def aggregate-function sum)

;;;;;;
;;; Count(*)

(def function sql-count-* ()
  ;; TODO the sql-all-columns ctor macro is not yet defined here (select.lisp)
  (sql-count (make-instance 'sql-all-columns)))

;;;;;;
;;; false expression

;; TODO delme and move to perec
(def syntax-node sql-false-expression (sql-query-expression)
  ()
  (:format-sql-syntax-node
   (format-sql-literal
    (sql-literal :value #f :type (make-instance 'sql-boolean-type)))))
