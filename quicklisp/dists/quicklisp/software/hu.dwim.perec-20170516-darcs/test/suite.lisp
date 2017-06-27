;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def function drop-all-test-tables ()
  (with-transaction
    (mapc [drop-table !1 :cascade #t]
          (collect-if [starts-with-subseq "_" !1]
                      (list-tables)))))

(def with-macro with-and-without-caching-slot-values ()
  (without-caching-slot-values
    (-body-))
  (with-caching-slot-values
    (-body-))
  (values))

(def macro with-two-transactions (instance-factory &body body)
  `(bind ((-instance- (with-transaction
                        ,instance-factory)))
     (with-transaction
       (revive-instance -instance-)
       ,@body)))

(def macro with-one-and-two-transactions (instance-factory &body body)
  (with-unique-names (instance-factory-fn body-fn)
    `(flet ((,instance-factory-fn ()
              ,instance-factory)
            (,body-fn (-instance-)
              (declare (ignorable -instance-))
              ,@body))
       (with-transaction
         (bind ((-instance- (,instance-factory-fn)))
           (,body-fn -instance-)))
       (with-two-transactions (,instance-factory-fn)
         (,body-fn -instance-)))))

(def function retest ()
  (drop-all-test-tables)
  (clear-compiled-query-cache)
  ;; TODO should take care of possible remaining persistent-object-hs
  (mapc (lambda (elememnt)
          (awhen (primary-table-of elememnt)
            (invalidate-computed-slot it 'ensure-exported))
          (when (typep elememnt 'persistent-class)
            (awhen (direct-instances-identity-view-of elememnt)
              (invalidate-computed-slot it 'ensure-exported))
            (awhen (direct-instances-data-view-of elememnt)
              (invalidate-computed-slot it 'ensure-exported))
            (awhen (all-instances-identity-view-of elememnt)
              (invalidate-computed-slot it 'ensure-exported))
            (awhen (all-instances-data-view-of elememnt)
              (invalidate-computed-slot it 'ensure-exported))))
        (append (hash-table-values *persistent-classes*)
                (hash-table-values *persistent-associations*)))
  (test))

(def suite (test :in root-suite) (&key (with-logging #f))
  (if with-logging
      (-run-child-tests-)
      (bind ((original-level (log-level/runtime 'root-logger)))
        (setf (log-level/runtime 'root-logger) +info+)
        (unwind-protect
             (-run-child-tests-)
          (setf (log-level/runtime 'root-logger) original-level)))))

(def suite (test/backend :in test))

(def suite (test/persistence :in test/backend))

(def suite (test/persistence/association :in test/persistence))

(def suite (test/query :in test/backend))

(def suite (test/dimensional :in test/backend))

;; test dimension
(def persistent-class* dimension-test ()
  ())

(def dimension test :type dimension-test)

(def fixture test-dimension-fixture
  (with-transaction
    (purge-instances 'dimension-test)
    (make-instance 'dimension-test)
    (make-instance 'dimension-test)
    (make-instance 'dimension-test))
  (-body-))
