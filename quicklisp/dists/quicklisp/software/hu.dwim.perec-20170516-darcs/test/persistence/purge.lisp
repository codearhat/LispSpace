;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite* (test/persistence/purge :in test/persistence))

(def persistent-class* purge-a-test ()
  ())

(def persistent-class* purge-b-test (purge-a-test)
  ())

(def persistent-class* purge-c-test (purge-a-test)
  ())

(def persistent-class* purge-d-test (purge-b-test purge-c-test)
  ()
  (:abstract #t))

(def persistent-class* purge-e-test (purge-d-test)
  ())

(def persistent-class* purge-f-test (purge-d-test)
  ())

(def persistent-class* purge-g-test (purge-b-test)
  ())

(def fixture fixture/persistent/purge
  (with-transaction
    (purge-instances 'purge-a-test)
    (make-instance 'purge-a-test)
    (make-instance 'purge-b-test)
    (make-instance 'purge-c-test)
    (make-instance 'purge-e-test)
    (make-instance 'purge-f-test)
    (make-instance 'purge-g-test))
  (-body-))

(def function purge-test (class-name delete-counter)
  (with-fixture fixture/persistent/purge
    (with-transaction
      (let ((last-delete-counter (delete-counter-of (hu.dwim.rdbms::command-counter-of *transaction*)))
            (total-instance-counter (length (select-instances purge-a-test)))
            (purged-instance-counter (count-if [typep !1 class-name] (select-instances purge-a-test))))
        (purge-instances class-name)
        (is (= (- (delete-counter-of (hu.dwim.rdbms::command-counter-of *transaction*)) last-delete-counter)
               delete-counter))
        (is (= (count-if [typep !1 class-name] (select-instances purge-a-test))
               0))
        (is (= (- total-instance-counter purged-instance-counter)
               (length (select-instances purge-a-test))))))))

(def definer purge-test (name class-name delete-counter)
  `(def test ,name ()
    (purge-test ',class-name ,delete-counter)))

(def purge-test test/persistence/purge/a purge-a-test 7)

(def purge-test test/persistence/purge/b purge-b-test 7)

(def purge-test test/persistence/purge/c purge-c-test 6)

(def purge-test test/persistence/purge/d purge-d-test 6)

(def purge-test test/persistence/purge/e purge-e-test 5)

(def purge-test test/persistence/purge/f purge-f-test 5)

(def purge-test test/persistence/purge/g purge-g-test 3)

(def test test/persistence/purge/sequence-of-instances ()
  (bind ((instances (with-transaction
                      (purge-instances 'purge-a-test)
                      (vector (make-instance 'purge-a-test)
                              (make-instance 'purge-b-test)
                              (make-instance 'purge-c-test)))))
    (with-transaction
      (is (length= 3 (select-instances purge-a-test)))
      (purge-instances instances))
    (with-transaction
      (is (length= 0 (select-instances purge-a-test)))
      (finishes (purge-instances (list))))))
