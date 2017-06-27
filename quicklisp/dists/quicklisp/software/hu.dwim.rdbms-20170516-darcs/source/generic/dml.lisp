;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms)

(def function sql-literal-values-for (columns values)
  (map 'list (lambda (column value)
               (if (or (typep value 'sql-literal)
                       ;; FIXME a style-warning because SQL-COLUMN is defined in something we are a dependency of
                       (not (typep column 'sql-column)))
                   value
                   ;; FIXME a style-warning because SQL-LITERAL is defined in something we are a dependency of
                   (make-instance 'sql-literal
                                  :value value
                                  :type (type-of column))))
       columns values))

(def function insert-record (table columns values)
  (nth-value 1 (execute (make-instance 'sql-insert
                                       :table table
                                       :columns columns
                                       :values (sql-literal-values-for columns values)))))

(def function update-records (table columns values &optional where)
  (nth-value 1 (execute (make-instance 'sql-update
                                       :table table
                                       :columns columns
                                       :values (sql-literal-values-for columns values)
                                       :where where))))

(def function delete-records (table &optional where)
  (nth-value 1 (execute (make-instance 'sql-delete
                                       :table table
                                       :where where))))

(def function select-records (columns tables &key where group-by having order-by offset limit)
  (execute (make-instance 'sql-select
                          :columns columns
                          :tables tables
                          :where where
                          :group-by group-by
                          :having having
                          :order-by order-by
                          :offset offset
                          :limit limit)))

(def function select-count-* (tables &optional where)
  (first-elt (first-elt (execute (make-instance 'sql-select
                                                :columns (list (sql-count-*))
                                                :tables tables
                                                :where where)))))
