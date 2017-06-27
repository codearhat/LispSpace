;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite* (test/query/select/association/1-1 :in test/query/select/association))

(def persistent-class* super-brother-test ()
  ())
   
(def persistent-class* super-sister-test ()
  ())

(def persistent-association*
  ((:class super-brother-test :slot sister :type (or null super-sister-test))
   (:class super-sister-test :slot brother :type (or null super-brother-test))))

(def persistent-class* sub-brother-test (super-brother-test)
  ())
   
(def persistent-class* sub-sister-test (super-sister-test)
  ())

;; FIXME: without filnalizing here, MOP gets into infinite loop
;;   when calculates the type of the slot in the sub-brother-test (compute-effective-slots)
;;   it calls compute-effective-slots on sub-sister-test and vice versa.
(eval-when (:load-toplevel :execute)
  (ensure-finalized (find-class 'super-brother-test))
  (ensure-finalized (find-class 'super-sister-test))
  (ensure-finalized (find-class 'sub-brother-test))
  (ensure-finalized (find-class 'sub-sister-test)))

(def persistent-association*
  ((:class sub-brother-test :slot sister :type (or null sub-sister-test))
   (:class sub-sister-test :slot brother :type (or null sub-brother-test))))

(def macro with-sister-and-brother-in-transaction (&body body)
  `(with-transaction
    (purge-instances 'sub-brother-test)
    (purge-instances 'sub-sister-test)
    (bind ((sister (make-instance 'sub-sister-test))
           (brother (make-instance 'sub-brother-test)))
      ,@body)))

(def test test/query/select/association/1-1/1 ()
  (with-sister-and-brother-in-transaction
      (is (null (select (s)
                  (from (s sub-sister-test))
                  (where (eq s (sister-of brother))))))
      (is (null (select (b)
                  (from (b sub-brother-test))
                  (where (eq b (brother-of sister))))))))

(def test test/query/select/association/1-1/2 ()
  (with-sister-and-brother-in-transaction
      (is (equal (select (s)
                   (from (s sub-sister-test))
                   (where (null (brother-of s))))
                 (list sister)))
      (is (equal (select (b)
                   (from (b sub-brother-test))
                   (where (null (sister-of b))))
                 (list brother)))))

(def test test/query/select/association/1-1/3 ()
  (with-sister-and-brother-in-transaction
    (setf (sister-of brother) sister)
    (is (equal (select (s)
                 (from (s sub-sister-test))
                 (where (eq s (sister-of brother))))
               (list sister)))
    (is (equal (select (b)
                 (from (b sub-brother-test))
                 (where (eq b (brother-of sister))))
               (list brother)))))

(def test test/query/select/association/1-1/4 ()
  (with-sister-and-brother-in-transaction
    (setf (sister-of brother) sister)
    (is (null (select (s)
                (from (s sub-sister-test))
                (where (null (brother-of s))))))
    (is (null (select (b)
                (from (b sub-brother-test))
                (where (null (sister-of b))))))))
