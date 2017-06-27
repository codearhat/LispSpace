;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

;;;;;;
;;; Slot

(def suite* (test/dimensional/slot :in test/dimensional))

(def persistent-class* dimensional-complex-test ()
  ((slot 0 :type integer-32)
   (test-dimension-dependent-slot 0 :type integer-32 :dimensions (test))
   (time-dependent-slot 0 :type integer-32 :dimensions (time))
   (validity-dependent-slot 0 :type integer-32 :dimensions (validity))
   (time-and-validity-dependent-slot 0 :type integer-32 :dimensions (time validity))))

(def persistent-class* dimensional-complex-unbound-test ()
  ((slot :type (or unbound integer-32))
   (test-dimension-dependent-slot 0 :type (or unbound integer-32) :dimensions (test))
   (time-dependent-slot :type (or unbound integer-32) :dimensions (time))
   (validity-dependent-slot :type (or unbound integer-32) :dimensions (validity))
   (time-and-validity-dependent-slot :type (or unbound integer-32) :dimensions (time validity))))

(def persistent-class* dimensional-complex-null-test ()
  ((slot :type (or null integer-32))
   (test-dimension-dependent-slot 0 :type (or null integer-32) :dimensions (test))
   (time-dependent-slot :type (or null integer-32) :dimensions (time))
   (validity-dependent-slot :type (or null integer-32) :dimensions (validity))
   (time-and-validity-dependent-slot :type (or null integer-32) :dimensions (time validity))))

(def persistent-class* dimensional-complex-unbound-or-null-test ()
  ((slot :type (or unbound null integer-32))
   (test-dimension-dependent-slot 0 :type (or unbound null integer-32) :dimensions (test))
   (time-dependent-slot :type (or unbound null integer-32) :dimensions (validity))
   (validity-dependent-slot :type (or unbound null integer-32) :dimensions (validity))
   (time-and-validity-dependent-slot :type (or unbound null integer-32) :dimensions (time validity))))

(def persistent-class* dimensional-complex-slot-test ()
  ((slot :type (or null integer-32))))

(def persistent-class* dimensional-complex-time-dependent-slot-test ()
  ((time-dependent-slot :type (or null integer-32) :dimensions (time))))

(def persistent-class* dimensional-complex-validity-dependent-slot-test ()
  ((validity-dependent-slot :type (or null integer-32) :dimensions (validity))))

(def persistent-class* dimensional-complex-time-and-validity-dependent-slot-test ()
  ((time-and-validity-dependent-slot :type (or null integer-32) :dimensions (time validity))))

(def persistent-class* dimensional-complex-inheritance-test
    (dimensional-complex-slot-test dimensional-complex-time-dependent-slot-test dimensional-complex-validity-dependent-slot-test dimensional-complex-time-and-validity-dependent-slot-test)
  ()
  (:metaclass persistent-class-d))

(def test test/dimensional/slot/normal ()
  (with-fixture test-dimension-fixture
    (run-complex-tests :class-name 'dimensional-complex-test
                       :instance-count 1)))

(def test test/dimensional/slot/unbound ()
  (with-fixture test-dimension-fixture
    (run-complex-tests :class-name 'dimensional-complex-unbound-test
                       :instance-count 1)))

(def test test/dimensional/slot/null ()
  (with-fixture test-dimension-fixture
    (run-complex-tests :class-name 'dimensional-complex-null-test
                       :instance-count 1)))

(def test test/dimensional/slot/unbound-or-null ()
  (with-fixture test-dimension-fixture
    (run-complex-tests :class-name 'dimensional-complex-unbound-or-null-test
                       :instance-count 1)))

(def test test/dimensional/slot/inheritance ()
  (with-fixture test-dimension-fixture
    (run-complex-tests :class-name 'dimensional-complex-inheritance-test
                       :instance-count 1)))
