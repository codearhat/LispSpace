;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

;;;;;;
;;; 1-1 association

(def suite* (test/dimensional/association :in test/dimensional))

(def suite* (test/dimensional/association/1-1 :in test/dimensional/association))

(def persistent-class* dimensional-brother-test ()
  ())
   
(def persistent-class* dimensional-sister-test ()
  ())

(def persistent-association*
  ((:class dimensional-sister-test :slot brother :type (or null dimensional-brother-test))
   (:class dimensional-brother-test :slot sister :type (or null dimensional-sister-test))))

(def persistent-association*
  ((:class dimensional-sister-test :slot time-dependent-brother :type (or null dimensional-brother-test))
   (:class dimensional-brother-test :slot time-dependent-sister :type (or null dimensional-sister-test)))
  (:dimensions (time)))

(def persistent-association*
  ((:class dimensional-sister-test :slot validity-dependent-brother :type (or null dimensional-brother-test) :cache #t)
   (:class dimensional-brother-test :slot validity-dependent-sister :type (or null dimensional-sister-test) :cache #t))
  (:dimensions (validity)))

(def persistent-association*
  ((:class dimensional-sister-test :slot time-and-validity-dependent-brother :type (or null dimensional-brother-test))
   (:class dimensional-brother-test :slot time-and-validity-dependent-sister :type (or null dimensional-sister-test)))
  (:dimensions (time validity)))

(def test test/dimensional/association/1-1/normal ()
  (run-complex-tests :class-names '(dimensional-brother-test dimensional-sister-test)
                     :slot-names '(sister brother)))

(def test test/dimensional/association/1-1/time-dependent ()
  (run-complex-tests :class-names '(dimensional-brother-test dimensional-sister-test)
                    :slot-names '(time-dependent-sister time-dependent-brother)))

(def test test/dimensional/association/1-1/validity-dependent ()
  (run-complex-tests :class-names '(dimensional-brother-test dimensional-sister-test)
                     :slot-names '(validity-dependent-sister validity-dependent-brother)))

(def test test/dimensional/association/1-1/time-and-validity-dependent ()
  (run-complex-tests :class-names '(dimensional-brother-test dimensional-sister-test)
                     :slot-names '(time-and-validity-dependent-sister time-and-validity-dependent-brother)))

(def test test/dimensional/association/1-1/store-value/2 ()
  (bind ((*simplify-d-values* #t))
    (with-transaction
     (bind ((brother (make-instance 'dimensional-brother-test))
            (sister1 (make-instance 'dimensional-sister-test))
            (sister2 (make-instance 'dimensional-sister-test)))
       (setf (sister-of brother) sister1)
       (setf (brother-of sister2) brother)
       (is (eq sister2 (sister-of brother)))
       (is (eq brother (brother-of sister2)))
       (is (null (brother-of sister1)))))))

(def test test/dimensional/association/1-1/integrity () 
  (bind ((brother-1 (with-transaction (make-instance 'dimensional-brother-test)))
         (sister-1 (with-transaction (make-instance 'dimensional-sister-test)))
         (sister-2 (with-transaction (make-instance 'dimensional-sister-test)))
         (brother-2 (with-transaction (make-instance 'dimensional-brother-test))))
    (with-transaction
      (with-revived-instances (brother-1 sister-1 sister-2 brother-2)
        (with-time (parse-datestring "2002-01-01")
          (with-validity-range "2002-01-01" +end-of-time+
            (setf (slot-value sister-1 'time-and-validity-dependent-brother) brother-1))
          ;; this clears the sister-1's slot on the intersection of this and previous interval
          (with-validity-range "2001-01-01" "2002-12-31"
            (setf (slot-value sister-2 'time-and-validity-dependent-brother) brother-1)))
        (with-time +beginning-of-time+
          (with-validity-range "2000-01-01" "2002-12-31"
            (setf (slot-value sister-1 'time-and-validity-dependent-brother) brother-2)))))
    (with-transaction
      (with-revived-instances (sister-1 brother-1 brother-2)
        (with-time (parse-datestring "2002-01-01")
          (with-validity-range +beginning-of-time+ +end-of-time+
            (is (d-value-equal
                 (slot-value sister-1 'time-and-validity-dependent-brother)
                 (make-d-value
                  '(time validity)
                  (list
                   (list (make-empty-coordinate-range (parse-datestring "2002-01-01"))
                         (make-coordinate-range 'ie +beginning-of-time+ (parse-datestring "2000-01-01")))
                   (list (make-empty-coordinate-range (parse-datestring "2002-01-01"))
                         (make-coordinate-range 'ie (parse-datestring "2000-01-01") (parse-datestring "2002-01-01")))
                   (list (make-empty-coordinate-range (parse-datestring "2002-01-01"))
                         (make-coordinate-range 'ie (parse-datestring "2002-01-01") (parse-datestring "2003-01-01")))
                   (list (make-empty-coordinate-range (parse-datestring "2002-01-01"))
                         (make-coordinate-range 'ie (parse-datestring "2003-01-01") +end-of-time+)))
                  (list nil brother-2 nil brother-1))))))))))

(def test test/dimensional/association/1-1/cache/validity-dependent ()
  (test/dimensional/association/cache
   (find-persistent-association
    'dimensional-sister-test~validity-dependent-brother~dimensional-brother-test~validity-dependent-sister)))

#+nil
(def test test/dimensional/association/1-1/cache/time-dependent ()
  (test/dimensional/association/cache
   (find-persistent-association
    'dimensional-sister-test~time-dependent-brother~dimensional-brother-test~time-dependent-sister)))

#+nil
(def test test/dimensional/association/1-1/cache/time-and-validity-dependent ()
  (test/dimensional/association/cache
   (find-persistent-association
    'dimensional-sister-test~time-and-validity-dependent-brother~dimensional-brother-test~time-and-validity-dependent-sister)))
