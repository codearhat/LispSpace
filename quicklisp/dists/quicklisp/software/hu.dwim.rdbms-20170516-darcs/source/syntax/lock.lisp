;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms)

(def syntax-node sql-lock-table (sql-dml-statement)
  ((table
       :type sql-identifier*)
   (mode
    :exclusive
    :type (member :row-share :row-exclusive :share-update :share :share-row-exclusive :exclusive))
   (wait
    #t
    :type boolean))
  (:documentation "An SQL LOCK TABLE statement.")
  (:format-sql-syntax-node
   (format-string "LOCK TABLE ")
   (format-sql-identifier table)
   (format-string " IN ")
   (format-string
    (ecase mode
      (:row-share "ROW SHARE")
      (:row-exclusive "ROW EXCLUSIVE")
      (:share-update "SHARE UPDATE")
      (:share "SHARE")
      (:share-row-exclusive "SHARE ROW EXCLUSIVE")
      (:exclusive "EXCLUSIVE")))
   (format-string " MODE ")
   (unless wait
     (format-string "NOWAIT"))))
