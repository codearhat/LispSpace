;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms.test)

(def special-variable *oracle-database* (make-instance 'hu.dwim.rdbms.oracle:oracle
                                                       :connection-specification '(:datasource "(ADDRESS =
                                                                                                  (PROTOCOL = TCP)
                                                                                                  (HOST = localhost)
                                                                                                  (PORT = 1521))"
                                                                                   :user-name "hu.dwim.rdbms.test"
                                                                                   :password "engedjbe")))


(def test (test/oracle :in test) ()
  (with-database *oracle-database*
    (test/backend)))
