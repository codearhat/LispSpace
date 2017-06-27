;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

;;;;;;
;;; Time-Dependent and time dependent

(def special-variable *time-dependent-validity-dependent-class-name*)

(def suite* (test/dimensional/time-dependent-validity-dependent :in test/dimensional))

(def persistent-class* time-dependent-validity-dependent-unbound-test ()
  ((population :type (or unbound integer-32) :dimensions (time validity))))

(def persistent-class* time-dependent-validity-dependent-null-test ()
  ((population :type (or null integer-32) :dimensions (time validity))))

(def macro with-time-dependent-validity-dependent-test-classes (&body forms)
  (with-unique-names (body)
    `(flet ((,body ()
              ,@forms))
       (bind ((*time-dependent-validity-dependent-class-name* 'time-dependent-validity-dependent-unbound-test))
         (,body))
       (bind ((*time-dependent-validity-dependent-class-name* 'time-dependent-validity-dependent-null-test))
         (,body)))))

(def test test/dimensional/time-dependent-validity-dependent/table ()
  (with-time-dependent-validity-dependent-test-classes
    (ensure-finalized (find-class *time-dependent-validity-dependent-class-name*))
    (is (null (columns-of (find-slot *time-dependent-validity-dependent-class-name* 'population))))))

(def test test/dimensional/time-dependent-validity-dependent/initial-value/unbound ()
  (with-transaction
    (signals unbound-slot-d (population-of (make-instance 'time-dependent-validity-dependent-unbound-test)))))

(def test test/dimensional/time-dependent-validity-dependent/initial-value/null ()
  (with-transaction
    (is (null (single-d-value
               (population-of (make-instance 'time-dependent-validity-dependent-null-test)))))))

(def test test/dimensional/time-dependent-validity-dependent/initial-value/integer ()
  (with-transaction
    (with-time-dependent-validity-dependent-test-classes
      (is (= 1000
             (single-d-value
              (population-of (make-instance *time-dependent-validity-dependent-class-name* :population 1000))))))))

(def test test/dimensional/time-dependent-validity-dependent/store-value/1 ()
  (with-time-dependent-validity-dependent-test-classes
    (with-validity "2007-01-01"
      (with-one-and-two-transactions
          (bind ((instance (make-instance *time-dependent-validity-dependent-class-name*)))
            (setf (population-of instance) 1000)
            instance)
        (is (= 1000 (single-d-value (population-of -instance-))))
        (is (= 1 (length (h-instances-of -instance-))))))))
