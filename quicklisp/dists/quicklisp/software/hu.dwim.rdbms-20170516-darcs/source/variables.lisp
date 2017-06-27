;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms)

(def (special-variable e :documentation "The current database, unbound by default. Can be rebound to access multiple databases within nested dynamic scope.")
  *database*)

(def (special-variable e :documentation  "The current transaction, unbound by default. Can be rebound to access multiple transactions within nested dynamic scope.")
  *transaction*)

(def (special-variable e) *implicit-transaction* #f
  "Specificies what to do when there is no transaction in progress during executing an SQL command. The value #f means an error will be signalled. The value :new unconditionally wraps each execution with a new transaction, while :ensure uses the current transaction if there's one.")

(def (special-variable e) *implicit-transaction-default-terminal-action* :rollback
  "Specifies the default terminal action for implicit transactions.")

(def special-variable *signal-non-destructive-alter-table-commands* (production-only t))
