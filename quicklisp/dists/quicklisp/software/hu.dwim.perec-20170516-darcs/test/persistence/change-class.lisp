;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite* (test/persistence/change-class :in test/persistence))

(def persistent-class* change-class-old-test ()
  ((kept-slot :type integer-16)
   (removed-slot :type integer-16)))

(def persistent-class* change-class-new-test ()
  ((kept-slot :type integer-16)
   (added-slot :type integer-16)
   (initialized-slot 4 :type integer-16)))

(def test test/persistence/change-class/no-change ()
  (bind ((instance (with-transaction
                     (purge-instances 'change-class-old-test)
                     (make-instance 'change-class-old-test :kept-slot 1 :removed-slot 2))))
    (with-transaction
      (revive-instance instance)
      (change-class instance 'change-class-old-test :added-slot 3))
    (with-transaction
      (revive-instance instance)
      (is (length= 1 (select-instances change-class-old-test)))
      (is (typep instance 'change-class-old-test))
      (is (eq instance (first (select-instances change-class-old-test))))
      (is (= 1 (kept-slot-of instance)))
      (is (= 2 (removed-slot-of instance))))))

(def test test/persistence/change-class/unrelated ()
  (bind ((instance (with-transaction
                     (purge-instances 'change-class-old-test)
                     (purge-instances 'change-class-new-test)
                     (make-instance 'change-class-old-test :kept-slot 1 :removed-slot 2))))
    (with-transaction
      (revive-instance instance)
      (change-class instance 'change-class-new-test :added-slot 3))
    (with-transaction
      (revive-instance instance)
      (is (null (select-instances change-class-old-test)))
      (is (typep instance 'change-class-new-test))
      (is (eq instance (first (select-instances change-class-new-test))))
      (is (= 1 (kept-slot-of instance)))
      (is (= 3 (added-slot-of instance)))
      (is (= 4 (initialized-slot-of instance))))))

(def persistent-class* change-class-superclass-test ()
  ((inherited-slot :type integer-16)
   (overridden-slot :type integer-16)))

(def persistent-class* change-class-subclass-test (change-class-superclass-test)
  ((overridden-slot :type integer-16)
   (extra-slot :type integer-16)))

(def test test/persistence/change-class/subclass ()
  (bind ((instance (with-transaction
                     (purge-instances 'change-class-superclass-test)
                     (purge-instances 'change-class-subclass-test)
                     (make-instance 'change-class-superclass-test :inherited-slot 1 :overridden-slot 2))))
    (with-transaction
      (revive-instance instance)
      (change-class instance 'change-class-subclass-test :extra-slot 3))
    (with-transaction
      (revive-instance instance)
      (is (length= 1 (select-instances change-class-superclass-test)))
      (is (typep instance 'change-class-subclass-test))
      (is (eq instance (first (select-instances change-class-subclass-test))))
      (is (= 1 (inherited-slot-of instance)))
      (is (= 2 (overridden-slot-of instance)))
      (is (= 3 (extra-slot-of instance))))))

(def test test/persistence/change-class/superclass ()
  (bind ((instance (with-transaction
                     (purge-instances 'change-class-superclass-test)
                     (purge-instances 'change-class-subclass-test)
                     (make-instance 'change-class-subclass-test :inherited-slot 1 :overridden-slot 2 :extra-slot 3))))
    (with-transaction
      (revive-instance instance)
      (change-class instance 'change-class-superclass-test :extra-slot 3))
    (with-transaction
      (revive-instance instance)
      (is (null (select-instances change-class-subclass-test)))
      (is (typep instance 'change-class-superclass-test))
      (is (eq instance (first (select-instances change-class-superclass-test))))
      (is (= 1 (inherited-slot-of instance)))
      (is (= 2 (overridden-slot-of instance))))))

(def persistent-class* change-class-association-test ()
  ())

(def persistent-association*
  ((:class change-class-association-test :slot sister :type (or null change-class-association-test))
   (:class change-class-association-test :slot brother :type (or null change-class-association-test))))

(def persistent-association*
  ((:class change-class-association-test :slot parent :type (or null change-class-association-test))
   (:class change-class-association-test :slot children :type (set change-class-association-test))))

(def persistent-association*
  ((:class change-class-association-test :slot members :type (set change-class-association-test))
   (:class change-class-association-test :slot groups :type (set change-class-association-test))))

(def test test/persistence/change-class/association ()
  (bind ((instance
          (with-transaction
            (purge-instances 'change-class-association-test)
            (bind ((sister (make-instance 'change-class-association-test))
                   (parent (make-instance 'change-class-association-test))
                   (children (list (make-instance 'change-class-association-test)
                                   (make-instance 'change-class-association-test)))
                   (members (list (make-instance 'change-class-association-test)
                                  (make-instance 'change-class-association-test)))
                   (groups (list (make-instance 'change-class-association-test)
                                 (make-instance 'change-class-association-test))))
              (make-instance 'change-class-association-test
                             :sister sister
                             :parent parent
                             :children children
                             :members members
                             :groups groups)))))
    (with-transaction
      (revive-instance instance)
      (change-class instance 'change-class-association-test))
    (with-transaction
      (revive-instance instance)
      (is (not (null (sister-of instance))))
      (is (not (null (parent-of instance))))
      (is (length= 2 (children-of instance)))
      (is (length= 2 (members-of instance)))
      (is (length= 2 (groups-of instance))))))
