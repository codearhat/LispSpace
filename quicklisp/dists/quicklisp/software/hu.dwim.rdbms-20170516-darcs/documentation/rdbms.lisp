;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms.documentation)

(def project :hu.dwim.rdbms)

(def method make-project-tab-pages ((component project/detail/inspector) (project (eql (find-project :hu.dwim.rdbms))))
  (append (list (tab-page/widget (:selector (icon/widget switch-to-tab-page :label "User guide"))
                  (make-value-inspector (find-book 'user-guide)))
                (tab-page/widget (:selector (icon/widget switch-to-tab-page :label "Dictionary"))
                  (make-value-inspector (mapcar 'find-dictionary '(database transaction transaction-hook execute command cursor table column view sequence index)))))
          (call-next-method)))

(def book user-guide (:title "User guide")
  (chapter (:title "Overview")
    (chapter (:title "What is hu.dwim.rdbms")
      (paragraph ()
        "A library to connect to various RDBMS systems.")
      (paragraph ()
        "There's at lease one known fork, which has a more developed Oracle (OCI) support at " (hu.dwim.uri:parse-uri "http://src.knowledgetools.de/tomas/porting/")))
    (chapter (:title "Why not something else?")
      ))
  (chapter (:title "Supported Platforms")
    (chapter (:title "Operating Systems")
      )
    (chapter (:title "Common Lisp Implementations")
      ))
  (chapter (:title "Instal Guide"))
  (chapter (:title "Connect to Database")
    (chapter (:title "Postgresql")
      "To create a 'test' database with a 'test' user:
su - postgres
createdb test
createuser -d -r -l -P test")
    (chapter (:title "Oracle")
      "dpkg -i oracle-xe-universal_10.2.0.1-1.0_i386.deb

rlwrap sqlplus / as sysdba

CREATE USER \"hu.dwim.rdbms.test\" IDENTIFIED BY \"engedjbe\";

When reinstalling an \"rm /etc/default/oracle-xe\" may ease things a lot."))
  (chapter (:title "Tutorial")
    )
  (chapter (:title "TODO")
    "The create/alter/drop table AST should really be structured like (create (table :name ... :columns ...)) and (alter (table ...)) where the (table ...) part is a shared table description. then a new format sub-protocols should be started for create and alter that works from the shared (table ...) ast node.

same applies to sql-create-index, sql-drop-index, sql-create-sequence, sql-drop-sequence

introduce a named-sql-syntax-node, add a print-object method for it.

:null should be converted back to cl:nil in the query results, except for boolean columns (the columns of the result
contain the type information necessary for this). this needs changes in hu.dwim.perec."
    (chapter (:title "with-database-connection")
      "hu.dwim.rdbms is missing a WITH-DATABASE-CONNECTION abstraction that could wrap several WITH-TRANSACTION blocks and also make (execute \"DROP DATABASE foo\") and prepare the path for connection pooling."))
  (chapter (:title "Concepts")
    (chapter (:title "Database")
      )
    (chapter (:title "Transaction")
      )))

(def dictionary database ()
  *database*
  database
  transaction-mixin-class
  with-database)

(def dictionary transaction ()
  *transaction*
  *implicit-transaction*
  *implicit-transaction-default-terminal-action*
  transaction
  begin
  begin-transaction
  call-in-transaction
  cleanup-transaction
  commit
  commit-transaction
  in-transaction-p
  make-transaction
  mark-transaction-for-commit-only
  mark-transaction-for-rollback-only
  rollback
  rollback-transaction
  transaction-error
  transaction-in-progress-p
  transaction-valid-p
  with-readonly-transaction
  with-transaction
  with-transaction*)

(def dictionary transaction-hook ()
  register-hook-in-transaction
  register-transaction-hook)

(def dictionary execute ()
  break-on-next-command
  current-delete-counter
  current-insert-counter
  current-select-counter
  current-update-counter
  execute
  execute-command
  execute-ddl
  report-transaction-state)

(def dictionary command ()
  delete-records
  insert-record
  select-count-*
  select-records
  update-records)

(def dictionary cursor ()
  collect-rows
  column-count
  column-name
  column-type
  column-value
  current-row
  cursor-position
  for-each-row
  make-cursor
  row-count)

(def dictionary table ()
  create-table
  create-temporary-table
  drop-table
  alter-table
  list-tables
  list-table-columns
  table-exists-p
  update-table)

(def dictionary column ()
  add-column
  drop-column
  alter-column-type
  add-primary-key-constraint
  list-table-columns)

(def dictionary view ()
  create-view
  drop-view
  list-dependent-views
  list-views
  update-view
  view-exists-p)

(def dictionary sequence ()
  create-sequence
  drop-sequence
  list-sequences
  sequence-exists-p
  sequence-next)

(def dictionary index ()
  create-index
  drop-index
  list-table-indices
  update-index)
