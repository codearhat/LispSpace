;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def special-variable *postgresql-database* (make-instance 'postgresql/perec
                                                           :generated-transaction-class-name 'transaction
                                                           :default-result-type 'vector
                                                           :muffle-warnings #t
                                                           :connection-specification '(:database "hu.dwim.perec.test"
                                                                                       :user-name "hu.dwim.perec.test"
                                                                                       :password "engedjbe")))

(def test (test/postgresql :in test) ()
  (with-database *postgresql-database*
    (with-new-compiled-query-cache
      (test/backend))))
