;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

;;;;;;
;;; m-n association

(def suite* (test/dimensional/association/m-n :in test/dimensional/association))

(def persistent-class* dimensional-student-test ()
  ())
   
(def persistent-class* dimensional-course-test ()
  ())

(def persistent-association*
  ((:class dimensional-course-test :slot students :type (set dimensional-student-test))
   (:class dimensional-student-test :slot courses :type (set dimensional-course-test))))

(def persistent-association*
  ((:class dimensional-course-test :slot time-dependent-students :type (set dimensional-student-test))
   (:class dimensional-student-test :slot time-dependent-courses :type (set dimensional-course-test)))
  (:dimensions (time)))

(def persistent-association*
  ((:class dimensional-course-test :slot validity-dependent-students :type (set dimensional-student-test))
   (:class dimensional-student-test :slot validity-dependent-courses :type (set dimensional-course-test)))
  (:dimensions (validity)))

(def persistent-association*
  ((:class dimensional-course-test :slot time-and-validity-dependent-students :type (set dimensional-student-test))
   (:class dimensional-student-test :slot time-and-validity-dependent-courses :type (set dimensional-course-test)))
  (:dimensions (time validity)))

(def test test/dimensional/association/m-n/normal ()
  (run-complex-tests :class-names '(dimensional-student-test dimensional-course-test)
                     :slot-names '(students courses)))

(def test test/dimensional/association/m-n/time-dependent ()
  (run-complex-tests :class-names '(dimensional-student-test dimensional-course-test)
                     :slot-names '(time-dependent-students time-dependent-courses)))

(def test test/dimensional/association/m-n/validity-dependent ()
  (run-complex-tests :class-names '(dimensional-student-test dimensional-course-test)
                     :slot-names '(validity-dependent-students validity-dependent-courses)))

(def test test/dimensional/association/m-n/time-and-validity-dependent ()
  (run-complex-tests :class-names '(dimensional-student-test dimensional-course-test)
                     :slot-names '(time-and-validity-dependent-students time-and-validity-dependent-courses)))

(def test test/dimensional/association/m-n/cache ()
  (test/dimensional/association/cache
   (find-persistent-association
    'dimensional-course-test~validity-dependent-students~dimensional-student-test~validity-dependent-courses)))
