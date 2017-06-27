;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms.test)

(def special-variable *postgresql-database* (make-instance 'hu.dwim.rdbms.postgresql:postgresql
                                                           :connection-specification '(:database "hu.dwim.rdbms.test"
                                                                                       :user-name "hu.dwim.rdbms.test"
                                                                                       :password "engedjbe")))

(def test (test/postgresql :in test) ()
  (with-database *postgresql-database*
    (test/backend)))
