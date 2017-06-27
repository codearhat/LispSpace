;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

;;;;;;
;;; Time dependent

(def special-variable *time-dependent-class-name*)

(def suite* (test/dimensional/time-dependent :in test/dimensional))

(def persistent-class* time-dependent-unbound-test ()
  ((population :type (or unbound integer-32) :dimensions (time))))

(def persistent-class* time-dependent-null-test ()
  ((population :type (or null integer-32) :dimensions (time))))

(def macro with-time-dependent-test-classes (&body forms)
  (with-unique-names (body)
    `(flet ((,body ()
              ,@forms))
       (bind ((*time-dependent-class-name* 'time-dependent-unbound-test))
         (,body))
       (bind ((*time-dependent-class-name* 'time-dependent-null-test))
         (,body)))))

(def test test/dimensional/time-dependent/table ()
  (with-time-dependent-test-classes
    (ensure-finalized (find-class *time-dependent-class-name*))
    (is (null (columns-of (find-slot *time-dependent-class-name* 'population))))))

(def test test/dimensional/time-dependent/initial-value/unbound ()
  (with-transaction
    (signals unbound-slot-d (population-of (make-instance 'time-dependent-unbound-test)))))

(def test test/dimensional/time-dependent/initial-value/null ()
  (bind ((*simplify-d-values* #t))
    (with-transaction
     (is (null (population-of (make-instance 'time-dependent-null-test))))
     (is (null (population-of (make-instance 'time-dependent-null-test :population nil)))))))

(def test test/dimensional/time-dependent/initial-value/integer ()
  (bind ((*simplify-d-values* #t))
    (with-transaction
      (with-time-dependent-test-classes
        (is (= 1000 (population-of (make-instance *time-dependent-class-name* :population 1000))))))))

(def test test/dimensional/time-dependent/store-value/1 ()
  (bind ((*simplify-d-values* #t))
    (with-time-dependent-test-classes
      (with-one-and-two-transactions
          (let ((instance (make-instance *time-dependent-class-name*)))
            (setf (population-of instance) 1000)
            instance)
        (is (= 1000 (population-of -instance-)))
        (is (= 1 (length (h-instances-of -instance-))))))))

(def test test/dimensional/time-dependent/store-value/2 ()
  (bind ((*simplify-d-values* #t))
    (with-time-dependent-test-classes
      (bind (((:values instance time)
              (with-transaction
                (values (make-instance *time-dependent-class-name* :population 1000)
                        *time-begin*))))
        (with-transaction
          (with-revived-instance instance
            (setf (population-of instance) 2000)))
        (with-transaction
          (with-revived-instance instance
            (is (= 2000 (population-of instance)))))
        (with-transaction
          (with-time time
            (with-revived-instance instance
              (is (= 1000 (population-of instance)))
              (is (= 2 (length (h-instances-of instance)))))))))))

(def test test/dimensional/time-dependent/cache ()
  (bind ((*simplify-d-values* #t))
    (with-time-dependent-test-classes
      (bind ((instance
              (with-transaction
                (make-instance *time-dependent-class-name* :population 1000))))
        (with-transaction
          (with-revived-instance instance
            (with-time-from *time-begin*
              (population-of instance))
            (setf (population-of instance) 2000)
            (with-time (adjust-timestamp *time-begin* (offset :sec 1))
              (is (= 2000 (population-of instance))))))))))

(def persistent-class* time-dependent-complex-test ()
  ((slot :type (or null integer-32))
   (slot-1 :type (or null integer-32) :dimensions (time))
   (slot-2 :type (or null integer-32) :dimensions (time))))

(def test test/dimensional/time-dependent/complex/same-time ()
  (bind ((*simplify-d-values* #t))
    (with-one-and-two-transactions
        (bind ((instance
                (make-instance 'time-dependent-complex-test :slot 0 :slot-1 1000)))
          (setf (slot-2-of instance) 2000)
          instance)
      (is (= 0 (slot-of -instance-)))
      (is (= 1000 (slot-1-of -instance-)))
      (is (= 2000 (slot-2-of -instance-)))
      (is (= 1 (length (h-instances-of -instance-)))))))

(def test test/dimensional/time-dependent/complex/different-time ()
  (bind ((*simplify-d-values* #t))
    (bind ((instance
            (with-transaction
              (make-instance 'time-dependent-complex-test :slot 0 :slot-1 1000))))
      (with-transaction
        (with-revived-instance instance
          (is (= 0 (slot-of instance)))
          (is (= 1000 (slot-1-of instance)))
          (is (null (slot-2-of instance)))
          (setf (slot-2-of instance) 2000)
          (is (= 2 (length (h-instances-of instance))))))
      (with-transaction
        (with-revived-instance instance
          (is (= 0 (slot-of instance)))
          (is (= 1000 (slot-1-of instance)))
          (is (= 2000 (slot-2-of instance))))))))
