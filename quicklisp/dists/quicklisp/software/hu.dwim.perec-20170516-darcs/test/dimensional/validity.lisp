;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

;;;;;;
;;; Time dependent

(def special-variable *validity-dependent-class-name*)

(def suite* (test/dimensional/validity-dependent :in test/dimensional))

(def persistent-class* validity-dependent-unbound-test ()
  ((population :type (or unbound integer-32) :dimensions (test validity))))

(def persistent-class* validity-dependent-null-test ()
  ((population :type (or null integer-32) :dimensions (test validity))))

(def macro with-validity-dependent-test-classes (&body forms)
  (with-unique-names (body)
    `(flet ((,body ()
              ,@forms))
       (bind ((*validity-dependent-class-name* 'validity-dependent-unbound-test))
         (,body))
       (bind ((*validity-dependent-class-name* 'validity-dependent-null-test))
         (,body)))))

(def test test/dimensional/validity-dependent/illegal-validity-errors ()
  (signals error
    (make-dimension-coordinate-range *validity-dimension* 'illegal-coordinate)))

(def test test/dimensional/validity-dependent/table ()
  (with-validity-dependent-test-classes
    (ensure-finalized (find-class *validity-dependent-class-name*))
    (is (null (columns-of (find-slot *validity-dependent-class-name* 'population))))))

(def test test/dimensional/validity-dependent/initial-value/unbound/1 ()
  (with-transaction
    (signals unbound-slot-d (population-of (make-instance 'validity-dependent-unbound-test)))))

(def test test/dimensional/validity-dependent/initial-value/unbound/2 ()
  (with-transaction
    (bind ((instance (make-instance 'validity-dependent-unbound-test)))
      (with-validity-to "2006"
        (setf (population-of instance) 1000))
      (with-validity-from "2008"
        (setf (population-of instance) 2000))
      (with-validity "2007"
        (signals unbound-slot-d (population-of instance)))
      (with-validity-range "2006" "2007"
        (signals unbound-slot-d (population-of instance)))
      (with-validity-range "2007" "2008"
        (signals unbound-slot-d (population-of instance))))))

(def test test/dimensional/validity-dependent/initial-value/null/1 ()
  (with-transaction
    (is (null (single-d-value
               (population-of (make-instance 'validity-dependent-null-test)))))))

(def test test/dimensional/validity-dependent/initial-value/null/2 ()
  (with-transaction
    (bind ((instance (make-instance 'validity-dependent-null-test)))
      (with-validity-to "2006"
        (setf (population-of instance) 1000))
      (with-validity-from "2008"
        (setf (population-of instance) 2000))
      (with-validity "2007"
        (is (null (single-d-value (population-of instance)))))
      (with-validity-range "2006" "2007"
        (is (d-value-equal
             (make-d-value
              '(test validity)
              (list (list +whole-domain-marker+
                          (make-coordinate-range
                           'ie (parse-timestring "2006-01-01TZ") (parse-timestring "2007-01-01TZ")))
                    (list +whole-domain-marker+
                          (make-coordinate-range
                           'ie (parse-timestring "2007-01-01TZ") (parse-timestring "2008-01-01TZ"))))
              '(1000 nil))
             (population-of instance))))
      (with-validity-range "2007" "2008"
        (is (d-value-equal
             (make-d-value
              '(test validity)
              (list (list +whole-domain-marker+
                          (make-coordinate-range
                           'ie (parse-timestring "2007-01-01TZ") (parse-timestring "2008-01-01TZ")))
                    (list +whole-domain-marker+
                          (make-coordinate-range
                           'ie (parse-timestring "2008-01-01TZ") (parse-timestring "2009-01-01TZ"))))
              '(nil 2000))
             (population-of instance)))))))

(def test test/dimensional/validity-dependent/initial-value/integer/1 ()
  (with-validity-dependent-test-classes
    (with-validity "2007-01-01"
      (with-one-and-two-transactions
          (make-instance *validity-dependent-class-name* :population 1000)
        (is (= 1000 (single-d-value (population-of -instance-))))))))

(def test test/dimensional/validity-dependent/store-value/1 ()
  (with-validity-dependent-test-classes
    (with-validity "2007-01-01"
      (with-one-and-two-transactions
          (bind ((instance (make-instance *validity-dependent-class-name*)))
            (setf (population-of instance) 1000)
            instance)
        (is (= 1000 (single-d-value (population-of -instance-))))))))

(def test test/dimensional/validity-dependent/store-value/2 ()
  (with-validity-dependent-test-classes
    (with-validity "2007-01-01"
      (with-one-and-two-transactions
          (bind ((instance (make-instance *validity-dependent-class-name*)))
            (setf (population-of instance) 1000)
            (is (= (update-counter-of (hu.dwim.rdbms::command-counter-of *transaction*)) 1))
            (is (= (insert-counter-of (hu.dwim.rdbms::command-counter-of *transaction*)) (+ 1 1)))
            instance)
        (let ((update-counter (update-counter-of (hu.dwim.rdbms::command-counter-of *transaction*)))
              (insert-counter (insert-counter-of (hu.dwim.rdbms::command-counter-of *transaction*))))
          (setf (population-of -instance-) 1000)
          (is (= (update-counter-of (hu.dwim.rdbms::command-counter-of *transaction*)) (1+ update-counter)))
          (is (= (insert-counter-of (hu.dwim.rdbms::command-counter-of *transaction*)) insert-counter)))))))

(def test test/dimensional/validity-dependent/store-value/3 ()
  (with-validity-dependent-test-classes
    (with-one-and-two-transactions
        (bind ((instance (make-instance *validity-dependent-class-name*)))
          (with-validity "2007"
            (setf (population-of instance) 1000))
          instance)
      (with-validity "2007-07"
        (setf (population-of -instance-) 2000))
      (with-validity "2007-07"
        (is (= 2000 (single-d-value (population-of -instance-)))))
      (with-validity "2007-07-01"
        (is (= 2000 (single-d-value (population-of -instance-)))))
      (with-validity "2007-06"
        (is (= 1000 (single-d-value (population-of -instance-)))))
      (with-validity "2007-08"
        (is (= 1000 (single-d-value (population-of -instance-))))))))

(def test test/dimensional/validity-dependent/d-value ()
  (with-one-and-two-transactions
      (bind ((instance (make-instance 'validity-dependent-unbound-test)))
        (with-validity "2006"
          (setf (population-of instance) 2006))
        instance)
    (with-validity "2007"
      (setf (population-of -instance-) 2007))
    (with-validity-range "2006" "2007"
      (iter (for ((test-instances validity-range) value) :in-d-value (population-of -instance-))
            (is (member value '(2006 2007) :test '=))
            (is (whole-domain-marker-p test-instances))
            (case value
              (2006 (is (eq 'ie (coordinate-range-bounds validity-range)))
                    (is (timestamp= (parse-datestring "2006-01-01") (coordinate-range-begin validity-range)))
                    (is (timestamp= (parse-datestring "2007-01-01") (coordinate-range-end validity-range))))
              (2007 (is (eq 'ie (coordinate-range-bounds validity-range)))
                    (is (timestamp= (parse-datestring "2007-01-01") (coordinate-range-begin validity-range)))
                    (is (timestamp= (parse-datestring "2008-01-01") (coordinate-range-end validity-range)))))))))

(def persistent-class* validity-dependent-complex-test ()
  ((slot :type (or null integer-32))
   (slot-1 :type (or null integer-32) :dimensions (validity))
   (slot-2 :type (or null integer-32) :dimensions (validity))))

(def test test/dimensional/validity-dependent/complex/same-validity ()
  (with-one-and-two-transactions
      (with-validity "2007"
        (bind ((instance
                (make-instance 'validity-dependent-complex-test :slot 0 :slot-1 1000)))
          (setf (slot-2-of instance) 2000)
          instance))
    (with-validity "2007"
      (is (= 0 (slot-of -instance-)))
      (is (= 1000 (single-d-value (slot-1-of -instance-))))
      (is (= 2000 (single-d-value (slot-2-of -instance-))))
      (is (= 1 (length (h-instances-of -instance-)))))))

(def test test/dimensional/validity-dependent/complex/different-validity ()
  (bind ((instance
          (with-transaction
            (with-validity "2007-01"
              (make-instance 'validity-dependent-complex-test :slot 0 :slot-1 1000)))))
    (with-transaction
      (with-validity "2007-02"
        (with-revived-instance instance
          (is (= 0 (slot-of instance)))
          (is (null (single-d-value (slot-1-of instance))))
          (is (null (single-d-value (slot-2-of instance))))
          (setf (slot-2-of instance) 2000)
          (is (= 2 (length (h-instances-of instance)))))))
    (with-transaction
      (with-validity "2007-01"
        (with-revived-instance instance
          (is (= 0 (slot-of instance)))
          (is (= 1000 (single-d-value (slot-1-of instance))))
          (is (is (null (single-d-value (slot-2-of instance))))))))))
