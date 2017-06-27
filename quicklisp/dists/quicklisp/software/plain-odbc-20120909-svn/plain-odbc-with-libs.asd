;;; -*- Mode: Lisp -*-

(defpackage :plain-odbc-with-libs 
  (:use :cl :asdf))

(let ((*package* (find-package :plain-odbc-with-libs)))
  (dolist (filename '("libs/cffi_0.10.5/cffi.asd"
                      "libs/babel_0.3.0/babel.asd"
                      "libs/trivial-features_0.6/trivial-features.asd"
                      "libs/alexandria/alexandria.asd"
                      "plain-odbc.asd"))
    (load (merge-pathnames filename *load-truename*))))
  
(asdf:defsystem "plain-odbc-with-libs"
                :depends-on (:plain-odbc))

