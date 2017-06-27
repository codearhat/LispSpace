;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;;;
;;; MOP methods

;; allows persistent keyword argument for persistent-direct-slot-definitions according to CLOS mop
;; even though there is no such slot in the class
(def method shared-initialize :around ((slot persistent-direct-slot-definition) slot-names
                                      &rest args &key persistent &allow-other-keys)
  (declare (ignore persistent))
  (apply #'call-next-method slot slot-names args))

(def method make-instance ((instance identity-preserving-class) &key instance &allow-other-keys)
  ;; used in class finalization protocol when instantiating direct slot definitions
  ;; this allows associations to be defined independently of direct slot definitions
  ;; and ensure-class to be called without loosing the old non association direct slot definitions
  (aif instance
       it
       (call-next-method)))

;; NOTE: allows to have standard slots within a persistent-class and prevents defpassociation to kill these slots
(def method make-instance :around ((class (eql (find-class 'standard-direct-slot-definition))) &key instance &allow-other-keys)
  (aif instance
       it
       (call-next-method)))

(def method initialize-instance :around ((class persistent-class) &rest args)
  (apply #'shared-initialize-around-persistent-class class #'call-next-method args))

(def method reinitialize-instance :around ((class persistent-class) &rest args)
  ;; update type dependencies first
  (mapc [deletef (depends-on-of !1) class]
        (depends-on-me-of class))
  (setf (depends-on-me-of class) nil)
  ;; emulate shared initialize which is not allowed to be overridden
  (apply #'shared-initialize-around-persistent-class class #'call-next-method :reinitialize #t :name (class-name class) args))

(def method reinitialize-instance :before ((association persistent-association) &key &allow-other-keys)
  (mapc [deletef (depends-on-of !1) association]
        (associated-classes-of association)))

(def method shared-initialize :after ((association persistent-association) slot-names &key &allow-other-keys)
  (mapc [pushnew association (depends-on-of !1)]
        (associated-classes-of association)))

(def method validate-superclass ((subclass persistent-class) (superclass standard-class))
  (subtypep (class-of subclass) (class-of superclass)))

(def method direct-slot-definition-class ((class persistent-class)
                                         &rest args &key instance persistent association &allow-other-keys)
  (cond (instance
         (class-of instance))
        (association
         (apply #'direct-slot-definition-class association args))
        (persistent
         (find-class 'persistent-direct-slot-definition))
        (t
         (call-next-method))))

(def method direct-slot-definition-class ((class persistent-association) &key &allow-other-keys)
  (find-class 'persistent-association-end-direct-slot-definition))

(def method effective-slot-definition-class ((class persistent-class)
                                            &rest args &key instance persistent association &allow-other-keys)
  (cond (instance
         (class-of instance))
        (association
         (apply #'effective-slot-definition-class association args))
        (persistent
         (find-class 'persistent-effective-slot-definition))
        (t
         (call-next-method))))

(def method effective-slot-definition-class ((class persistent-association) &key &allow-other-keys)
  (find-class 'persistent-association-end-effective-slot-definition))

(def method compute-effective-slot-definition ((class persistent-class) slot-name direct-slot-definitions)
  (declare (ignore slot-name))
  (if (some (lambda (slot)
              (typep slot 'persistent-direct-slot-definition))
            direct-slot-definitions)
      (bind ((standard-initargs (compute-standard-effective-slot-definition-initargs class direct-slot-definitions))
             (slot-initargs (compute-persistent-effective-slot-definition-initargs class direct-slot-definitions))
             (initargs (append slot-initargs standard-initargs))
             (effective-slot-class (apply #'effective-slot-definition-class class :persistent #t initargs)))
        (prog1-bind effective-slot-definition
            (if (subtypep effective-slot-class 'persistent-effective-slot-definition)
                (apply #'make-instance effective-slot-class :direct-slots direct-slot-definitions initargs)
                (apply #'make-instance effective-slot-class initargs))
          (setf (slot-value effective-slot-definition '%%class) class)
          (bind ((type (canonical-type-of effective-slot-definition))
                 (normalized-type (normalized-type-for type))
                 (mapped-type (mapped-type-for normalized-type))
                 (unbound-subtype-p (unbound-subtype-p type))
                 (null-subtype-p (and (not (null-subtype-p mapped-type))
                                      (null-subtype-p type)))
                 (initfunction (slot-definition-initfunction effective-slot-definition)))
            (when (and (or null-subtype-p
                           (set-type-p* type))
                       (not unbound-subtype-p)
                       (not initfunction))
              (setf #*((:allegro
                        (slot-value effective-slot-definition 'excl::initfunction))
                       (:clozure
                        (slot-value effective-slot-definition 'ccl::initfunction))
                       (t
                        (slot-definition-initfunction effective-slot-definition)))
                   (constantly nil))))))
      (call-next-method)))

(def function compute-standard-effective-slot-definition-initargs (class direct-slot-definitions)
  #*((:sbcl
      (sb-pcl::compute-effective-slot-definition-initargs class direct-slot-definitions))
     (:allegro
      (excl::compute-effective-slot-definition-initargs class direct-slot-definitions))
     (:clozure
      (bind ((initer (dolist (s direct-slot-definitions)
                       (when (ccl::%slot-definition-initfunction s)
                         (return s))))
             (documentor (dolist (s direct-slot-definitions)
                           (when (ccl::%slot-definition-documentation s)
                             (return s))))
             (first (car direct-slot-definitions))
             (initargs (let* ((initargs nil))
                         (dolist (dslot direct-slot-definitions initargs)
                           (dolist (dslot-arg (ccl::%slot-definition-initargs  dslot))
                             (pushnew dslot-arg initargs :test #'eq))))))
        (list
         :name (c2mop:slot-definition-name (car direct-slot-definitions))
         :allocation (ccl::%slot-definition-allocation first)
         :documentation (when documentor (nth-value
                                          1
                                          (ccl::%slot-definition-documentation
                                           documentor)))
         :class (ccl::%slot-definition-class first)
         :initargs initargs
         :initfunction (if initer (ccl::%slot-definition-initfunction initer))
         :initform (if initer (ccl::%slot-definition-initform initer))
         :type (ccl::dslotd-type-intersection direct-slot-definitions))))
     (t (not-yet-implemented/crucial-api))))

(def function compute-persistent-effective-slot-definition-initargs (class direct-slot-definitions)
  (iter (for slot-option-name in (delete-duplicates
                                  (collect-if [not (eq (symbol-package !1) (find-package :common-lisp))]
                                              (mapcan [mapcar #'slot-definition-name
                                                              (class-slots (class-of !1))]
                                                      direct-slot-definitions))))
        (bind ((specific-direct-slot-definitions
                (collect-if [find slot-option-name (class-slots (class-of !1)) :key #'slot-definition-name]
                            direct-slot-definitions)))
          (appending
           (compute-persistent-effective-slot-definition-option class
                                                                (first (sort (copy-list specific-direct-slot-definitions)
                                                                             [subtypep (class-of !1) (class-of !2)]))
                                                                slot-option-name
                                                                specific-direct-slot-definitions)))))

(def generic compute-persistent-effective-slot-definition-option (class direct-slot slot-option-name direct-slot-definitions)
  (:method ((class persistent-class)
            direct-slot-definition
            slot-option-name
            direct-slot-definitions)
    nil)

  (:method ((class persistent-class)
            (direct-slot persistent-direct-slot-definition)
            slot-option-name
            direct-slot-definitions)
    (if (member slot-option-name '(cache prefetch index unique type-check))
        (some [slot-initarg-and-value !1 slot-option-name] direct-slot-definitions)
        (call-next-method)))

  (:method ((class persistent-class)
            (direct-slot persistent-association-end-direct-slot-definition)
            slot-option-name
            direct-slot-definitions)
    (if (member slot-option-name '(association))
        (some [slot-initarg-and-value !1 slot-option-name] direct-slot-definitions)
        (call-next-method))))

(def method finalize-inheritance :after ((class persistent-class))
  (invalidate-inheritance class)
  (mapc [ensure-slot-reader* class !1]
        (collect-if [set-type-p* (canonical-type-of !1)]
                    (persistent-effective-slots-of class)))
  (bind ((class-name (class-name class)))
    (setf (class-id->class-name (id-of class)) class-name))
  (setf (standard-instance-access (class-prototype class) (slot-definition-location (find-slot class 'persistent))) #f))

(def method compute-slots :after ((class persistent-class))
  "Invalidates the cached slot values whenever the effective slots are recomputed, so that all dependent computed state will be invalidated and recomputed when requested."
  (invalidate-computed-slot class 'standard-direct-slots)
  (invalidate-computed-slot class 'standard-effective-slots))

(def function invalidate-inheritance (class)
  (invalidate-computed-slot class 'persistent-direct-superclasses)
  (invalidate-computed-slot class 'persistent-direct-subclasses)
  (mapc [invalidate-computed-slot !1 'persistent-direct-superclasses]
        (persistent-direct-subclasses-of class))
  (mapc [invalidate-computed-slot !1 'persistent-direct-subclasses]
        (persistent-direct-superclasses-of class)))

;;;;;;
;;; Utility

(def generic persistent-class-default-superclasses (class &key name direct-slots direct-superclasses &allow-other-keys)
  (:method ((class persistent-class) &key name direct-superclasses &allow-other-keys)
    (unless (or (eq name 'persistent-object)
                (find-if (lambda (direct-superclass)
                           (ignore-errors (subtypep direct-superclass (find-class 'persistent-object))))
                         direct-superclasses))
      (list (find-class 'persistent-object)))))

;; TODO used only at one place, move into flet
(def function process-direct-slot-definitions (direct-slots)
  (loop for direct-slot :in direct-slots
     collect (if (or (getf direct-slot :instance)
                     (getf direct-slot :persistent))
                 direct-slot
                 (if (hasf direct-slot :persistent)
                     ;; remove :persistent nil
                     (remove-from-plist direct-slot :persistent)
                     ;; add default :persistent t
                     (append direct-slot '(:persistent t))))))

(def function association-direct-slot-definitions (class)
  (when (slot-boundp class 'depends-on)
    (let ((depends-on-associations
           (collect-if [typep !1 'persistent-association]
                       (depends-on-of class))))
      (mappend (lambda (association)
                 (let ((association-end-definitions
                       (collect-if [eq (class-name class) (getf !1 :class)]
                                   (association-end-definitions-of association))))
                  (mapcar [append (list :name (getf !1 :slot)
                                        :association association
                                        :persistent #t)
                                  (remove-from-plist !1 :slot :class :accessor)]
                          association-end-definitions)))
               depends-on-associations))))

;; this is not the real shared-initialize because portable programs are not allowed to override that
;; so we are somewhat emulating it by calling this function from both initialize-instance and reinitialize-instance
(def function shared-initialize-around-persistent-class (class call-next-method &rest args
                                                         &key (reinitialize #f) name direct-slots (direct-superclasses nil direct-superclasses?) abstract direct-store id &allow-other-keys)
  ;; call initialize-instance or reinitialize-instance next method
  (prog1
      (apply call-next-method
             class
             :direct-slots (append (process-direct-slot-definitions direct-slots)
                                   (association-direct-slot-definitions class))
             (append (list :abstract (first abstract))
                     (list :direct-store direct-store)
                     (when (or (not reinitialize)
                               direct-superclasses?)
                       (list :direct-superclasses (append direct-superclasses
                                                          (persistent-class-default-superclasses class
                                                                                                 :name name
                                                                                                 :direct-slots direct-slots
                                                                                                 :direct-superclasses direct-superclasses))))
                     (when id
                       (list :id (first id)))
                     (remove-from-plist args :reinitialize :direct-slots :direct-superclasses :abstract)))
    (setf (find-persistent-class name) class)
    (invalidate-inheritance class)
    (invalidate-computed-slot class 'standard-direct-slots)
    (invalidate-computed-slot class 'standard-effective-slots)
    ;; update type specific class dependencies
    (mapc [bind ((type (canonical-type-of !1)))
            (when (set-type-p* type)
              (bind ((associated-class (find-class (set-type-class-for type))))
                (pushnew class (depends-on-of associated-class))
                (pushnew associated-class (depends-on-me-of class))))]
          (persistent-direct-slots-of class))
    (mapc [bind ((association (association-of !1))
                 (association-end-position
                  (position (slot-definition-name !1) (association-end-definitions-of association)
                            :key [getf !1 :slot])))
            (if (= 0 association-end-position)
                (setf (primary-association-end-of association) !1)
                (setf (secondary-association-end-of association) !1))]
          (collect-if [typep !1 'persistent-association-end-direct-slot-definition]
                      (class-direct-slots class)))))

(def function ensure-slot-reader* (class slot)
  (when-bind reader-name (reader-name-of slot)
    (bind ((reader (concatenate-symbol reader-name "*"))
           (reader-gf (ensure-generic-function reader :lambda-list '(instance))))
      (ensure-method reader-gf
                     `(lambda (instance)
                        (with-lazy-slot-value-collections
                          (slot-value-using-class ,class instance ,slot)))
                     :specializers (list class)))))

(def function slot-initarg-and-value (instance slot-name)
  (when (slot-boundp instance slot-name)
    (list (first (slot-definition-initargs (find-slot (class-of instance) slot-name)))
          (slot-value instance slot-name))))

(def function reader-name-of (effective-slot)
  (first (some #'slot-definition-readers (direct-slots-of effective-slot))))

(def function writer-name-of (effective-slot)
  (first (some #'slot-definition-writers (direct-slots-of effective-slot))))

(def function class-slots (class)
  (closer-mop:class-slots (ensure-finalized class)))
