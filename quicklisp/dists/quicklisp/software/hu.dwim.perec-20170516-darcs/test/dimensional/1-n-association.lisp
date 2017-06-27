;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

;;;;;;
;;; 1-n association

(def suite* (test/dimensional/association/1-n :in test/dimensional/association))

(def persistent-class* dimensional-parent-test ()
  ())
   
(def persistent-class* dimensional-child-test ()
  ())

(def persistent-association*
  ((:class dimensional-child-test :slot parent :type (or null dimensional-parent-test))
   (:class dimensional-parent-test :slot children :type (set dimensional-child-test))))

(def persistent-association*
  ((:class dimensional-child-test :slot time-dependent-parent :type (or null dimensional-parent-test))
   (:class dimensional-parent-test :slot time-dependent-children :type (set dimensional-child-test)))
  (:dimensions (time)))

(def persistent-association*
  ((:class dimensional-child-test :slot validity-dependent-parent :type (or null dimensional-parent-test))
   (:class dimensional-parent-test :slot validity-dependent-children :type (set dimensional-child-test)))
  (:dimensions (validity)))

(def persistent-association*
  ((:class dimensional-child-test :slot time-and-validity-dependent-parent :type (or null dimensional-parent-test))
   (:class dimensional-parent-test :slot time-and-validity-dependent-children :type (set dimensional-child-test)))
  (:dimensions (time validity)))

(def test test/dimensional/association/1-n/normal ()
  (run-complex-tests :class-names '(dimensional-parent-test dimensional-child-test)
                     :slot-names '(parent children)))

(def test test/dimensional/association/1-n/time-dependent ()
  (run-complex-tests :class-names '(dimensional-parent-test dimensional-child-test)
                     :slot-names '(time-dependent-parent time-dependent-children)))

(def test test/dimensional/association/1-n/validity-dependent ()
  (run-complex-tests :class-names '(dimensional-parent-test dimensional-child-test)
                     :slot-names '(validity-dependent-parent validity-dependent-children)))

(def test test/dimensional/association/1-n/time-and-validity-dependent ()
  (run-complex-tests :class-names '(dimensional-parent-test dimensional-child-test)
                     :slot-names '(time-and-validity-dependent-parent time-and-validity-dependent-children)))

(def test test/dimensional/association/1-n/cache ()
  (test/dimensional/association/cache
   (find-persistent-association
    'dimensional-child-test~validity-dependent-parent~dimensional-parent-test~validity-dependent-children)))
