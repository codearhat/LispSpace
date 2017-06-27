;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms.postgresql)

(def (class* e) postgresql (database)
  ((muffle-warnings #f :type boolean :accessor muffle-warnings?)))

(def special-variable *unique-counter* 0)

(def (function i) generate-unique-postgresql-name (base)
  (string+ (string base) (princ-to-string (incf *unique-counter*))))

(def constant +maximum-rdbms-name-length+ 63)

;; this name mapping is not injective, different lisp names _may_ be mapped to the same rdbms name
(def method calculate-rdbms-name ((db (eql :postgresql)) thing name)
  "Cuts off the end of names that are too long and appends the hash of the original name."
  (calculate-rdbms-name-with-utf-8-length-limit name +maximum-rdbms-name-length+ :prefix "_"))

(def method calculate-rdbms-name ((db postgresql) thing name)
  (calculate-rdbms-name :postgresql thing name))
