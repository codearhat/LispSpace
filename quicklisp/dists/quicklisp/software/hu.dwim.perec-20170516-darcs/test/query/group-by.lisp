;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite* (test/query/group-by :in test/query))

(def persistent-class* group-by-test ()
  ((int-attr :type (or null integer-32))
   (str-attr :type (or null (text 50)))
   (date-attr :type (or null date))))

(def persistent-class* group-by-child-test ()
  ((int-attr :type (or null integer-32))))

(def persistent-association*
  ((:class group-by-test :slot children :type (set group-by-child-test))
   (:class group-by-child-test :slot parent :type (or null group-by-test))))

(def fixture group-by-data
  (with-transaction
    (purge-instances 'group-by-test)
    (purge-instances 'group-by-child-test)
    (make-instance 'group-by-test
                   :int-attr 1
                   :str-attr "1"
                   :date-attr (parse-datestring "2001-01-01")
                   :children (list (make-instance 'group-by-child-test :int-attr 1)))
    (make-instance 'group-by-test
                   :int-attr 2
                   :str-attr "2"
                   :date-attr (parse-datestring "2001-01-02")
                   :children (list (make-instance 'group-by-child-test :int-attr 2)))
    (make-instance 'group-by-test
                   :int-attr 3
                   :str-attr "3"
                   :date-attr (parse-datestring "2001-01-03")
                   :children (list (make-instance 'group-by-child-test :int-attr 3)))
    (make-instance 'group-by-test
                   :int-attr nil
                   :str-attr nil
                   :date-attr nil
                   :children nil ))
  (-body-))

(def definer group-by-test (name (&rest args) &body body)
  `(def test ,name ,args
     (with-fixture group-by-data
       (with-transaction
         ,@body))))

(def group-by-test test/query/group-by/string ()
  (is
   (null
    (set-exclusive-or
     (select ((count (int-attr-of o)))
       (from (o group-by-test))
       (group-by (str-attr-of o)))
     '(0 1 1 1)
     :test 'equalp))))

(def group-by-test test/query/group-by/string/2 ()
  (is
   (null
    (set-exclusive-or
     (select ((str-attr-of o) (count (int-attr-of o)))
       (from (o group-by-test))
       (group-by (str-attr-of o)))
     '((nil 0) ("1" 1) ("2" 1) ("3" 1))
     :test 'equalp))))

(def group-by-test test/query/group-by/parent ()
  (bind ((p1 (select-first-matching-instance group-by-test (where (= (int-attr-of -instance-) 1))))
         (p2 (select-first-matching-instance group-by-test (where (= (int-attr-of -instance-) 2))))
         (p3 (select-first-matching-instance group-by-test (where (= (int-attr-of -instance-) 3)))))
    (is
     (null
      (set-exclusive-or
       (select (parent (max (int-attr-of child)))
         (from (parent group-by-test) (child group-by-child-test))
         (where (eq parent (parent-of child)))
         (group-by parent))
       `((,p1 1) (,p2 2) (,p3 3))
       :test 'equalp)))))

(def group-by-test test/query/group-by/parent/2 ()
  (bind ((p1 (select-first-matching-instance group-by-test (where (= (int-attr-of -instance-) 1))))
         (p2 (select-first-matching-instance group-by-test (where (= (int-attr-of -instance-) 2))))
         (p3 (select-first-matching-instance group-by-test (where (= (int-attr-of -instance-) 3)))))
    (is
     (null
      (set-exclusive-or
       (select ((parent-of child) (max (int-attr-of child)))
         (from (child group-by-child-test))
         (group-by (parent-of child)))
       `((,p1 1) (,p2 2) (,p3 3))
       :test 'equalp)))))
