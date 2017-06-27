;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

(def (class* e) oracle/perec (database-mixin hu.dwim.rdbms.oracle:oracle)
  ())

(def method create-temporary-table (table-name columns subselect (database hu.dwim.rdbms.oracle:oracle))
  (labels ((ensure-oracle-temporary-table-exists (table-name columns)
             (unless (oracle-temporary-table-exists-p table-name)
               (with-transaction (execute (sql-create-table
                                            :name table-name
                                            :temporary :delete-rows
                                            :columns columns)))))
           (oracle-temporary-table-exists-p (table-name)
             (execute (format nil
                              "select table_name from user_tables where lower(table_name)='~A' and temporary='Y'"
                              (string-downcase (rdbms-name-for table-name))) ;; TODO should be case sensitive
                      :result-type 'list)))
    (ensure-oracle-temporary-table-exists table-name columns)
    (sql-insert
      :table table-name
      :columns columns
      :subselect (sql-subquery :query subselect))))

(def method drop-temporary-table (table-name (database hu.dwim.rdbms.oracle:oracle))
  nil)
