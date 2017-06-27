;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite* (test/persistence/cache :in test/persistence) ()
  (with-caching-slot-values
    (-run-child-tests-))
  (without-caching-slot-values
    (-run-child-tests-)))

(def suite* (test/persistence/cache/slot-access :in test/persistence/cache))

(def function counter+ (counter value)
  (if *cache-slot-values* counter (+ counter value)))

(def persistent-class* cache-test ()
  ((name nil :type (or null (text 20)))))

(def test test/persistence/cache/slot/read-initial-value ()
  (with-transaction
    (bind ((object (make-instance 'cache-test :name "the one"))
           (select-counter (current-select-counter)))
      (name-of object)
      (is (= (counter+ select-counter 1) (current-select-counter))))))

(def test test/persistence/cache/slot/multiple-read ()
  (bind ((object
          (with-transaction
            (make-instance 'cache-test :name "the one")))
         select-counter)
    (with-transaction
      (revive-instance object)
      (name-of object)
      (setf select-counter (current-select-counter))
      (name-of object)
      (is (= (counter+ select-counter 1) (current-select-counter))))))

(def test test/persistence/cache/slot/write-and-read ()
  (with-transaction
    (bind ((object (make-instance 'cache-test))
           (select-counter))
      (setf (name-of object) "the one")
      (setf select-counter (current-select-counter))
      (name-of object)
      (is (= (counter+ select-counter 1) (current-select-counter))))))

(def test test/persistence/cache/invalidate ()
  (let ((instance (with-transaction (make-instance 'persistence-test :name "the one"))))
    (with-transaction
      (with-revived-instance instance
        (with-transaction
          (with-reloaded-instance instance
            (setf (name-of instance) "the other")))
        (invalidate-cached-instance instance)
        (is (equal "the other" (name-of instance)))))))

(def test test/persistence/cache/reference/read-initial-value ()
  (with-transaction
    (bind ((object (make-instance 'reference-test :referred (make-instance 'referred-test)))
           (select-counter (current-select-counter)))
      (referred-of object)
      (is (= (counter+ select-counter 1) (current-select-counter))))))

(def test test/persistence/cache/reference/multiple-read ()
  (bind ((object
          (with-transaction
            (make-instance 'reference-test :referred (make-instance 'referred-test))))
         select-counter)
    (with-transaction
      (revive-instance object)
      (referred-of object)
      (setf select-counter (current-select-counter))
      (referred-of object)
      (is (= (counter+ select-counter 1) (current-select-counter))))))

(def test test/persistence/cache/reference/write-and-read ()
  (with-transaction
    (bind ((object (make-instance 'reference-or-null-test))
           (select-counter))
      (setf (referred-of object) (make-instance 'referred-test))
      (setf select-counter (current-select-counter))
      (referred-of object)
      (is (= (counter+ select-counter 1) (current-select-counter))))))

(def test test/persistence/cache/association/1-1/read-initial-value ()
  (with-transaction
    (bind ((sister (make-instance 'sister-test))
           (brother (make-instance 'brother-test :sister sister))
           (select-counter (current-select-counter)))
      (sister-of brother)
      (brother-of sister)
      (is (= (counter+ select-counter 2) (current-select-counter))))))

(def test test/persistence/cache/association/1-1/multiple-read ()
  (bind (((sister brother)
          (with-transaction
            (bind ((sister (make-instance 'sister-test))
                   (brother (make-instance 'brother-test :sister sister)))
              (list sister brother))))
         select-counter)
    (with-transaction
      (revive-instance sister)
      (revive-instance brother)
      (sister-of brother)
      (brother-of sister)
      (setf select-counter (current-select-counter))
      (sister-of brother)
      (brother-of sister)
      (is (= (counter+ select-counter 2) (current-select-counter))))))

(def test test/persistence/cache/association/1-1/write-and-read ()
  (with-sister-and-brother-transaction
    (bind (select-counter)
      (setf (sister-of brother) sister)
      (setf select-counter (current-select-counter))
      (sister-of brother)
      (is (= (counter+ select-counter 1) (current-select-counter))))))

(def test test/persistence/cache/association/1-1/write-unbound ()
  (with-transaction
    (finishes
      (bind ((brother (make-instance 'unbound-brother-test)))
        (select (b)
          (from (b unbound-brother-test)))))
    (finishes
      (bind ((sister (make-instance 'unbound-sister-test)))
        (select (b)
          (from (b unbound-sister-test)))))))

(def test test/persistence/cache/association/1-n/read-initial-value ()
  (with-transaction
    (bind ((parent (make-instance 'parent-test))
           (child (make-instance 'child-test :parent parent))
           (select-counter (current-select-counter)))
      (parent-of child)
      (is (= (counter+ select-counter 1) (current-select-counter))))))

(def test test/persistence/cache/association/1-n/multiple-read ()
  (bind ((child
          (with-transaction
            (make-instance 'child-test :parent (make-instance 'parent-test))))
         select-counter)
    (with-transaction
      (revive-instance child)
      (parent-of child)
      (setf select-counter (current-select-counter))
      (parent-of child)
      (is (= (counter+ select-counter 1) (current-select-counter))))))

(def test test/persistence/cache/association/1-n/write-and-read ()
  (with-parent-and-child-transaction
    (bind (select-counter)
      (setf (parent-of child) parent)
      (setf select-counter (current-select-counter))
      (parent-of child)
      (is (= (counter+ select-counter 1) (current-select-counter))))))
