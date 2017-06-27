;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;;;
;;; Caching slot values in instances

(def (constant e :documentation "This value is stored in slots to indicate that the slot value is not cached.")
  +not-cached-slot-marker+ '+not-cached-slot-marker+)

(def (special-variable :documentation "True means slot values will be cached in the slots of the persistent instances. Writing a slot still goes directly to the database but it will be also stored in the instance. If the instance's state is modified in the database it is up to the modifier to clear the list of cached slots from the instance using the invalidate functions. The purpose of the slot value cache is to increase performance and reduce the number of database interactions during a transaction.")
  *cache-slot-values* #t)

(def special-variable *skip-storing-equal-slot-values* #f)

(def macro with-storing-equal-slot-values (&body forms)
  `(bind ((*skip-storing-equal-slot-values* #f))
     ,@forms))

(def macro without-storing-equal-slot-values (&body forms)
  `(bind ((*skip-storing-equal-slot-values* #t))
     ,@forms))

(def (function io) not-cached-slot-marker-p (value)
  (eq +not-cached-slot-marker+ value))

(def condition* instance-not-in-current-transaction (error)
  ((instance))
  (:report
   (lambda (condition stream)
     (bind ((instance (instance-of condition)))
       (format stream "Accessing a persistent ~A while it is ~A transaction."
               instance
               (if (instance-in-transaction-p instance)
                   "attached to another"
                   "not attached to the current"))))))

(def macro assert-instance-access (instance persistent)
  (check-type instance symbol)
  (check-type persistent symbol)
  `(progn
     (unless (or (not ,persistent)
                 (in-transaction-p))
       (error "Accessing a persistent ~A while there's no transaction in progress." ,instance))
     (unless (or (not ,persistent)
                 (instance-in-current-transaction-p ,instance))
       (restart-case
           (error 'instance-not-in-current-transaction :instance ,instance)
         (load-instance ()
           :report (lambda (stream)
                     (format stream "Load instance ~A into the current transaction and go on" ,instance))
           (setf ,instance (load-instance ,instance)
                 ,persistent (persistent-p ,instance)))))))

(def macro assert-instance-slot-correspondence ()
  `(debug-only
     #+nil(assert (eq (class-of instance) (persistent-slot-definition-class slot))))) ;; TODO THL wasn't portable, don't want this for non-persistent slots

(def generic propagate-cache-changes (class instance slot new-value)
  (:documentation "Partially invalidate or update the cache to reflect setting the slot of instance to new-value.")

  (:method ((class persistent-class) (instance persistent-object) (slot persistent-effective-slot-definition) new-value)
           (values)))

(def (function eo) invalidate-cached-instance (instance)
  "Invalidates all cached slot values in the instance."
  (bind ((class (class-of instance))
         (persistent? (persistent-p instance)))
    (assert-instance-access instance persistent?)
    (iter (for slot in (persistent-effective-slots-of class))
          (when (cache-p slot)
            (invalidate-cached-slot instance slot)))))

(def (function o) invalidate-all-cached-instances ()
  "Invalidates all cached slot values in the instance."
  (map-cached-instances #'invalidate-cached-instance))

(def (function io) invalidate-cached-slot (instance slot)
  "Invalidates the given cached slot value in the instance."
  (assert-instance-slot-correspondence)
  (setf (standard-instance-access instance (slot-definition-location slot)) +not-cached-slot-marker+))

(def (function o) copy-cached-slot-values (old-instance new-instance)
  "Copies slot values from OLD-INSTANCE to NEW-INSTANCE."
  (bind ((class (class-of old-instance)))
    (dolist (slot (persistent-effective-slots-of class))
      (bind (((:values cached-p old-value) (slot-value-cached-p old-instance slot)))
        (when (and cached-p
                   (not (slot-value-cached-p new-instance slot)))
          (bind ((association-end? (typep slot 'persistent-association-end-effective-slot-definition))
                 (to-one-association-end? (and association-end?
                                               (to-one-association-end-p slot)))
                 (old-value-is-a-persistent-object? (or (and to-one-association-end?
                                                        old-value
                                                        (not (unbound-slot-marker-p old-value)))
                                                   ;; or the slow path...
                                                   (typep old-value 'persistent-object)))
                 (new-value (cond (to-one-association-end?
                                   (when old-value-is-a-persistent-object?
                                     (load-instance old-value :skip-existence-check #t)))
                                  ((not old-value-is-a-persistent-object?)
                                   ;; protect against the non-association situation: (slot-name :type some-persistent-class)
                                   (debug-only (assert (not (typep old-value 'persistent-object))))
                                   old-value))))
            (when (or (not association-end?)
                      to-one-association-end?)
              (setf (underlying-slot-boundp-or-value-using-class class new-instance slot) new-value))
            ;; recurse after new-value is already cached
            (when to-one-association-end?
              (when old-value-is-a-persistent-object?
                (setf (persistent-p new-value) (persistent-p old-value))
                (copy-cached-slot-values old-value new-value)))))))))

(def (function io) slot-value-cached-p (instance slot)
  "Tells whether the given slot is cached in the instance or not."
  (assert-instance-slot-correspondence)
  (bind ((value (standard-instance-access instance (slot-definition-location slot))))
    (values
     (not (not-cached-slot-marker-p value))
     value)))

(def generic slot-value-equal-p (value-1 value-2)
  (:documentation "When a new slot value is set in a persistent slot it will be compared to the cached value and will not be propagated to the database when this function returns true.")

  (:method (value-1 value-2)
    (eq value-1 value-2))

  (:method ((value-1 number) (value-2 number))
    (= value-1 value-2))

  (:method ((value-1 string) (value-2 string))
    (string= value-1 value-2)))

;;;;;;
;;; Primitve slot value

(def (function io) underlying-slot-value (instance slot-name)
  "Similar to slot-value but never interacts with the database."
  (bind ((class (class-of instance))
         (slot (find-slot class slot-name)))
    (if slot
        (underlying-slot-value-using-class class instance slot)
        (values (slot-missing class instance slot-name 'slot-value)))))

(def (function io) (setf underlying-slot-value) (new-value instance slot-name)
  "Similar to (setf slot-value) but never interacts with the database."
  (bind ((class (class-of instance))
         (slot (find-slot class slot-name)))
    (if slot
        (setf (underlying-slot-value-using-class class instance slot) new-value)
        (progn
          (slot-missing class instance slot-name 'slot-value)
          new-value))))

(def (function io) underlying-slot-boundp-or-value (instance slot-name)
  "Similar to slot-value-boundp-or-value but never interacts with the database."
  (bind ((class (class-of instance))
         (slot (find-slot class slot-name)))
    (if slot
        (underlying-slot-boundp-or-value-using-class class instance slot)
        (values (slot-missing class instance slot-name 'slot-value)))))

(def (function io) (setf underlying-slot-boundp-or-value) (new-value instance slot-name)
  "Similar to (setf slot-value-boundp-or-value) but never interacts with the database."
  (bind ((class (class-of instance))
         (slot (find-slot class slot-name)))
    (if slot
        (setf (underlying-slot-boundp-or-value-using-class class instance slot) new-value)
        (progn
          (slot-missing class instance slot-name 'slot-value)
          new-value))))

(def (function io) underlying-slot-value-using-class (class instance slot)
  "Returns the cached value of the instance's slot similar to slot-value-using-class but never interacts with the database."
  (bind ((value (underlying-slot-boundp-or-value-using-class class instance slot)))
    (if (unbound-slot-marker-p value)
        (values (slot-unbound class instance (slot-definition-name slot)))
        value)))

(def (function io) (setf underlying-slot-value-using-class) (new-value class instance slot)
  "Sets the cached value of the instance's slot similar to (setf slot-value-using-class) but never interacts with the database."
  (setf (underlying-slot-boundp-or-value-using-class class instance slot) new-value))

(def (function io) underlying-slot-makunbound-using-class (class instance slot)
  "Makes the cached instance's slot unbound similar to slot-makunbound-using-class but never interacts with the database."
  (setf (underlying-slot-boundp-or-value-using-class class instance slot) +unbound-slot-marker+))

(def (function io) underlying-slot-boundp-using-class (class instance slot)
  "Returns the cached boundness of the instance's slot similar to slot-boundp-using-class but never interacts with the database."
  (not (unbound-slot-marker-p (underlying-slot-boundp-or-value-using-class class instance slot))))

(def (function io) underlying-slot-boundp-or-value-using-class (class instance slot)
  "Either returns the cached slot value or the unbound slot marker. This method does not interact with the database."
  (declare (ignorable class))
  (assert-instance-slot-correspondence)
  (prog1-bind value (standard-instance-access instance (slot-definition-location slot))
    (assert (not (not-cached-slot-marker-p value)))
    #+sbcl
    (debug-only
      (assert (not (eq value sb-pcl::+slot-unbound+))))))

(def (function io) (setf underlying-slot-boundp-or-value-using-class) (new-value class instance slot)
  "Either sets the slot value to the given new value or makes the slot unbound if the new value is the unbound marker. This method does not interact with the database."
  (declare (ignorable class))
  (assert-instance-slot-correspondence)
  (debug-only
    (assert (not (not-cached-slot-marker-p new-value)))
    #+sbcl
    (assert (not (eq new-value sb-pcl::+slot-unbound+))))
  (setf (standard-instance-access instance (slot-definition-location slot)) new-value))

(def (function eio) best-effort-slot-value (instance slot-designator)
  "Tries to get the slot value without signaling any errors. If there's a transaction and the slot is not cached, then fetches it from the db. Otherwise just blindly returns the value from the slot, including the symbols +NOT-CACHED-SLOT-MARKER+ and +UNBOUND-SLOT-MARKER+."
  (if (instance-in-current-transaction-p instance)
      (handler-case
          (slot-value instance slot-designator)
        (unbound-slot ()
          '+unbound-slot-marker+))
      (standard-instance-access instance (etypecase slot-designator
                                           (fixnum slot-designator)
                                           (effective-slot-definition (slot-definition-location slot-designator))
                                           (symbol (slot-definition-location (find-slot (class-of instance) slot-designator)))))))

(def compiler-macro best-effort-slot-value (&whole whole instance slot-designator)
  (if (and (consp slot-designator)
           (eq (first slot-designator) 'quote)
           (symbolp (second slot-designator)))
      (once-only (instance)
        `(if (instance-in-current-transaction-p ,instance)
             (handler-case
                 ;; so that the slot-value optimizations of the implementation can jump in here
                 (slot-value ,instance ,slot-designator)
               (unbound-slot ()
                 '+unbound-slot-marker+))
             (standard-instance-access ,instance (slot-definition-location (find-slot (class-of ,instance) ,slot-designator)))))
      whole))

;;;;;;
;;; CLOS MOP slot-value-using-class and friends

;; TODO: this svuc slows down slot access of standard slots, should solve this some other way (maybe specialize persistent slot?)
(def method slot-value-using-class ((class persistent-class)
                                   (instance persistent-object)
                                   (slot standard-effective-slot-definition))
  "Prefetches persistent slot values when determining whether the instance is persistent or not."
  (assert-instance-slot-correspondence)
  ;; check for the persistent flag slot
  (if (and (eq (slot-definition-name slot) 'persistent)
           (not (slot-boundp-using-class class instance slot)))
      ;; prefetch if possible otherwise simple existence check
      (if (prefetched-slots-of class)
          (bind (((:values restored-slot-values restored-slots) (restore-prefetched-slots class instance #t)))
            ;; the persistent flag must be stored prior to caching any slot value
            (prog1 (setf (slot-value-using-class class instance slot) (not (null restored-slots)))
              ;; cache prefetched slots
              (iter (for restored-slot-value in restored-slot-values)
                    (for restored-slot in restored-slots)
                    (when (and *cache-slot-values*
                               (cache-p restored-slot))
                      (setf (underlying-slot-boundp-or-value-using-class class instance restored-slot)
                            restored-slot-value)))))
          ;; simple existence test
          (setf (slot-value-using-class class instance slot) (instance-exists-in-database-p instance)))
      (call-next-method)))

(def (function io) slot-boundp-or-value-using-class (class instance slot return-with)
  (declare (type function return-with))
  (assert-instance-slot-correspondence)
  (bind ((persistent (persistent-p instance))
         ((:values slot-value-cached cached-value)
          (slot-value-cached-p instance slot)))
    (assert-instance-access instance persistent)
    (if (or (not persistent)
            (and *cache-slot-values*
                 slot-value-cached))
        (funcall return-with cached-value)
        ;; restore the slot and all other prefetched slots from the database
        (if (and *cache-slot-values*
                 (prefetch-p slot))
            ;; restore all prefetched slot values at once
            (bind (((:values restored-slot-values restored-slots) (restore-prefetched-slots class instance))
                   (slot-value))
              (iter (for restored-slot-value in restored-slot-values)
                    (for restored-slot in restored-slots)
                    (when (eq slot restored-slot)
                      (setf slot-value restored-slot-value))
                    (when (cache-p restored-slot)
                      (setf (underlying-slot-boundp-or-value-using-class class instance restored-slot)
                            restored-slot-value)))
              (funcall return-with slot-value))
            ;; only restore the requested slot from the database
            (bind (((:values restored-slot-value restored-slot) (restore-slot class instance slot)))
              (when (and *cache-slot-values*
                         (cache-p restored-slot))
                (setf (underlying-slot-boundp-or-value-using-class class instance restored-slot) restored-slot-value))
              (funcall return-with restored-slot-value))))))

(def (function io) (setf slot-boundp-or-value-using-class) (new-value class instance slot)
  (assert-instance-slot-correspondence)
  (bind ((persistent (persistent-p instance))
         (cache (cache-p slot)))
    (assert-instance-access instance persistent)
    (when (persistent-object-p new-value)
      (bind ((persistent-new-value? (persistent-p new-value)))
        (assert-instance-access new-value persistent-new-value?)))
    ;; always store the slot into the database
    (when persistent
      (bind (((:values slot-value-cached cached-value)
              (slot-value-cached-p instance slot)))
        (unless (and cache
                     slot-value-cached
                     *skip-storing-equal-slot-values*
                     (slot-value-equal-p cached-value new-value))
          (store-slot class instance slot new-value)
          (update-instance-cache-for-modified-instance instance))))
    (propagate-cache-changes class instance slot new-value)
    (when (or (not persistent)
              (and *cache-slot-values*
                   cache))
      (setf (underlying-slot-boundp-or-value-using-class class instance slot) new-value))
    new-value))

(def method slot-value-using-class ((class persistent-class)
                                   (instance persistent-object)
                                   (slot persistent-effective-slot-definition))
  "Reads the slot value from the database or the cache."
  (slot-boundp-or-value-using-class class instance slot
                                    [if (unbound-slot-marker-p !1)
                                        (slot-unbound class instance (slot-definition-name slot))
                                        !1]))

(def method (setf slot-value-using-class) (new-value
                                          (class persistent-class)
                                          (instance persistent-object)
                                          (slot persistent-effective-slot-definition))
  "Writes the new slot value to the database and the cache."
  #+nil
  (debug-only
    (assert (not (or (not-cached-slot-marker-p new-value)
                     (unbound-slot-marker-p new-value)))))
  (setf (slot-boundp-or-value-using-class class instance slot) new-value))

(def method slot-boundp-using-class ((class persistent-class)
                                    (instance persistent-object)
                                    (slot persistent-effective-slot-definition))
  "Reads boundness from the database or the cache."
  (slot-boundp-or-value-using-class class instance slot [not (unbound-slot-marker-p !1)]))

(def method slot-makunbound-using-class ((class persistent-class)
                                        (instance persistent-object)
                                        (slot persistent-effective-slot-definition))
  "Writes boundness to the database and the cache."
  (setf (slot-boundp-or-value-using-class class instance slot) +unbound-slot-marker+)
  instance)

(def method change-class :before ((instance persistent-object) (new-class standard-class) &key &allow-other-keys)
  (make-transient instance))

(def method update-instance-for-different-class :before ((old-instance persistent-object) (new-instance persistent-object) &key &allow-other-keys)
  (bind ((new-oid (class-id-and-instance-id->oid (id-of (class-of new-instance)) (oid-instance-id (oid-of old-instance)))))
    (setf (oid-of new-instance) new-oid)
    (setf (persistent-p new-instance) #f)
    (setf (transaction-of new-instance) nil)
    (setf (transaction-event-of new-instance) nil)))

(def method update-instance-for-different-class :after ((old-instance persistent-object) (new-instance persistent-object) &key &allow-other-keys)
  (make-persistent new-instance))
