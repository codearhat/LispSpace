;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite* (test/persistence/export :in test/persistence))

(def persistent-class* export-parent-test ()
  ())

(def persistent-class* export-child-test ()
  ())

(def persistent-association*
  ((:class export-parent-test :slot export-child-tests :type (set export-child-test))
   (:class export-child-test :slot export-parent-test :type export-parent-test)))

(def test test/persistence/export/associated-class ()
  (with-transaction
    (purge-instances 'export-parent-test)
    (purge-instances 'export-child-test)
    (make-instance 'export-child-test :export-parent-test (make-instance 'export-parent-test)))
  ;; NOTE: emulate a reload and the fact that the classes are not exported
  (dolist (class-name '(export-parent-test export-child-test))
    (bind ((class (find-class class-name)))
      (invalidate-computed-slot class 'hu.dwim.perec::ensure-exported)
      (remhash (hu.dwim.perec::id-of class) hu.dwim.perec::*oid-class-id->class-name-map*)
      (finalize-inheritance class)))
  (finishes
    (with-transaction
      (export-parent-test-of (select-first-matching-instance export-child-test)))))

(def persistent-class* authentication-instrument ()
  ())

(def persistent-class* named-audited-object ()
  ()
  ;; A possible workaround for the time being. The test machinery also needs it
  ;; to avoid triggering an assert in a sensitive state.
  (:id 42))

(def test test/persistence/export/class-id/bug1 ()
  (is (not (eql (hu.dwim.perec::id-of (find-class 'authentication-instrument))
                (hu.dwim.perec::id-of (find-class 'named-audited-object)))))
  (with-expected-failures
    ;; TODO FIXME
    (is (not (eql (hu.dwim.perec::compute-class-id (find-class 'authentication-instrument))
                  (hu.dwim.perec::compute-class-id (find-class 'named-audited-object)))))))
