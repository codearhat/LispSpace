;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite* (test/query/select/association :in test/query/select))

(def persistent-class* person-test ()
  ((name :type (text 50))))

(def persistent-class* movie-test ()
  ((title :type (text 50))))

(def persistent-class* man-test (person-test)
  ())

(def persistent-class* woman-test (person-test)
  ())

(def persistent-class* performer-test (person-test)
  ())

(def persistent-class* actor-test (performer-test man-test)
  ())

(def persistent-class* actress-test (performer-test woman-test)
  ())

(def persistent-association*
  ((:class man-test :slot wife :type (or null woman-test))
   (:class woman-test :slot husband :type (or null man-test))))

(def persistent-association*
  ((:class movie-test :slot performers :type (set performer-test))
   (:class performer-test :slot movies :type (set movie-test))))

(def fixture association-test-fixture
  (with-transaction
    (purge-instances 'movie-test)
    (purge-instances 'person-test)
    (let ((oceans-twelwe (make-instance 'movie-test :title "Ocean's Twelwe"))
          (mr&mrs-smith (make-instance 'movie-test :title "Mr. & Mrs. Smith"))
          (george-clooney (make-instance 'actor-test :name "George Clooney"))
          (brad-pitt (make-instance 'actor-test :name "Brad Pitt"))
          (angelina-jolie (make-instance 'actress-test :name "Angelina Jolie")))
      (setf (performers-of oceans-twelwe) (list george-clooney brad-pitt)
            (performers-of mr&mrs-smith) (list brad-pitt angelina-jolie)
            (wife-of brad-pitt) angelina-jolie)))
  (-body-))

(def test test/query/select/association-1-1/1 ()
  (test-query (:select-count 1 :record-count 1 :fixture association-test-fixture)
    (select (m)
      (from (m man-test))
      (where (and
              (not (null (wife-of m)))
              (equal (name-of (wife-of m)) "Angelina Jolie"))))))

(def test test/query/select/association-1-1/2 ()
  (test-query (:select-count 1 :record-count 1 :fixture association-test-fixture)
    (select (m w)
      (from (m man-test) (w woman-test))
      (where (eq (wife-of m) w))))
  (test-query (:select-count 1 :record-count 1 :fixture association-test-fixture)
    (select (m w)
      (from (m man-test) (w woman-test))
      (where (eq (husband-of w) m)))))

(def test test/query/select/association-n-m ()
  (test-query (:select-count 1 :record-count 4 :fixture association-test-fixture)
    (select ((title-of m) (name-of p))
      (from (m movie-test) (p performer-test))
      (where (member p (performers-of m))))))

(def test test/query/select/association-chain ()
  (test-query (:select-count nil :record-count 1 :fixture association-test-fixture) ;; TODO select-count
    (select (m)
      (from (m man-test))
      (where (and (not (null (wife-of m)))
                  (typep (husband-of (wife-of m)) 'performer-test)
                  (> (length (movies-of (husband-of (wife-of m)))) 0))))))

(def test test/query/select/association-m-n/lexical-1 ()
  (test-query (:select-count nil :record-count 2 :fixture association-test-fixture)
    (let ((s (select-first-matching-instance movie-test)))
      (select (p)
        (from (p performer-test))
        (where (member p (performers-of s)))))))

(def test test/query/select/association-m-n/lexical-2 ()
  (test-query (:select-count nil :record-count 2 :fixture association-test-fixture)
    (let ((s (select-first-matching-instance movie-test)))
      (select (p)
        (from (p performer-test))
        (where (member s (movies-of p)))))))
