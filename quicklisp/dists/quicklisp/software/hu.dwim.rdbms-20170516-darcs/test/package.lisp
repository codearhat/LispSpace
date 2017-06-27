;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :hu.dwim.rdbms.test
  (:use :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.logger
        :hu.dwim.rdbms
        :hu.dwim.stefil
        :hu.dwim.syntax-sugar
        :hu.dwim.util)

  ;; we intentionally not import all internal hu.dwim.rdbms symbols here to test the proper exporting of the symbols, too
  (:import-from :hu.dwim.rdbms
                #:*database*
                #:*sql-stream*
                #:*transaction*
                #:compile-sexp-sql
                #:compile-sexp-sql-binding-variable
                #:compile-sexp-sql-column
                #:compile-sexp-sql-columns
                #:compile-sexp-sql-literal
                #:compile-sexp-sql-type
                #:connection-specification-of
                #:eval-always
                #:expand-sql-ast-into-lambda-form
                #:rdbms.debug
                #:rdbms.dribble
                #:rdbms.error
                #:rdbms.info
                #:rdbms.warn
                #:rebind
                #:value-of
                #:with-transaction*)

  (:readtable-setup
   (setup-readtable/same-as-package :hu.dwim.rdbms)
   (hu.dwim.syntax-sugar:enable-string-quote-syntax)
   (hu.dwim.rdbms:enable-sql-syntax)))

(in-package :hu.dwim.rdbms.test)

(import-sql-syntax-node-names)

(import-sql-constructor-names)
