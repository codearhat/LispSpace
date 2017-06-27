;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :hu.dwim.rdbms.sqlite
  (:use :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.logger
        :hu.dwim.rdbms
        :hu.dwim.syntax-sugar
        :hu.dwim.util
        :local-time)
  (:shadowing-import-from #:hu.dwim.rdbms
                          #:as
                          #:columns
                          #:name
                          #:temporary
                          #:type
                          #:type-of)

  (:shadow #:null)
  (:readtable-setup (setup-readtable/same-as-package :hu.dwim.rdbms)))

(in-package :hu.dwim.rdbms.sqlite)

;; import all the internal symbol of :hu.dwim.rdbms into :hu.dwim.rdbms.sqlite
(do-symbols (symbol :hu.dwim.rdbms)
  (when (and (eq (symbol-package symbol) #.(find-package :hu.dwim.rdbms))
             (not (find-symbol (symbol-name symbol) #.(find-package :hu.dwim.rdbms.sqlite))))
    (import symbol)))

(cffi:define-foreign-library sqlite3
  (:unix (:or "libsqlite3.so.0" "libsqlite3.so"))
  (:windows "sqlite3.dll")
  (t (:default "libsqlite3")))
