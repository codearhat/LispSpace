;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms.sqlite)

(def method database-list-sequences ((database sqlite))
  (error "Not implemented"))

(def method database-list-tables ((database sqlite))
  (map 'list [elt !1 0]
       (execute "SELECT name FROM sqlite_master WHERE type = 'table'")))

(def method database-list-table-columns (name (database sqlite))
  (error "Not implemented"))

(def method database-list-table-indices (name (database sqlite))
  (map 'list
       (lambda (column)
         (make-instance 'sql-index
                        :name (first-elt column)
                        :table-name name))
       (execute
        (format nil "SELECT name FROM sqlite_master WHERE type = 'index' AND tbl_name = '~A'"
                (string-downcase name)))))
