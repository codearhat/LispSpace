;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def special-variable *sqlite-database* (make-instance 'hu.dwim.rdbms.sqlite:sqlite
                                                       :generated-transaction-class-name 'transaction
                                                       :default-result-type 'vector
                                                       :muffle-warnings #t
                                                       :transaction-mixin 'transaction-mixin
                                                       :connection-specification '(:file-name "/tmp/hu.dwim.perec.test")))

(def test (test/sqlite :in test) ()
  (with-database *sqlite-database*
    (with-new-compiled-query-cache
      (test/backend))))
