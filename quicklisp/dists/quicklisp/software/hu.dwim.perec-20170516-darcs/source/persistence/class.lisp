;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;;;
;;; Persistent class and slot meta objects

;; TODO: support flattenning subclasses into superclass and dispatch on type
;; TODO: support flattenning (1-1) associations and slots with persistent object subtype into referer's table

(def computed-class* persistent-class (standard-class exportable)
  ((id
    (compute-as (compute-class-id -self-))
    :type integer
    :documentation "A unique integer that identifies the persistent class. This integer will be part of the oid for each instance in the database")
   (abstract
    (compute-as #f)
    :type boolean
    :documentation "An abstract persistent class cannot be instantiated but still can be used in associations and may have slots. Calling make-instance on an abstract persistent class will signal an error. On the other hand abstract classes might not have a primary table and thus handling the instances of subclasses may require simpler or less SQL statements.")
   (direct-store
    (compute-as '(:separate))
    :type list
    :documentation "Specifies in which tables should the effective slots be stored. Valid options are :separate :push-down :push-up for all slots or per each superclass slot groups.")
   (effective-store
    (compute-as (compute-effective-store -self-))
    :type list
    :documentation "Merges the direct store class options according to the class precedence list.")
   (standard-direct-slots
    (compute-as (class-direct-slots -self-))
    :type (list standard-effective-slot-definition)
    :documentation "All computed slots that needs the direct slots should use this slot so that invalidation will work.")
   (standard-effective-slots
    (compute-as (class-slots -self-))
    :type (list standard-effective-slot-definition)
    :documentation "All computed slots that needs the effective slots should use this slot so that invalidation will work.")
   (persistent-direct-slots
    (compute-as (collect-if (of-type 'persistent-direct-slot-definition) (standard-direct-slots-of -self-)))
    :type (list persistent-direct-slot-definition)
    :documentation "The list of direct slots which are defined to be persistent in this class.")
   (persistent-effective-slots
    (compute-as (collect-if (of-type 'persistent-effective-slot-definition) (standard-effective-slots-of -self-)))
    :type (list persistent-effective-slot-definition)
    :documentation "The list of effective slots which are turned out to be persistent in this class.")
   (persistent-direct-superclasses
    (compute-as (collect-if #'persistent-class-p (class-direct-superclasses -self-)) )
    :type (list persistent-class)
    :documentation "The list of persistent direct subclasses.")
   (persistent-class-precedence-list
    (compute-as (list* -self- (persistent-effective-superclasses-of -self-)))
    :type (list persistent-class)
    :documentation "Similar to class-precedence-list but includes only persistent classes.")
   (persistent-effective-superclasses
    (compute-as (compute-persistent-effective-superclasses -self-))
    :type (list persistent-class)
    :documentation "The list of effective persistent superclasses in class precedence order.")
   (persistent-direct-subclasses
    ;; TODO CLASS-DIRECT-SUBCLASSES is not cooperating with the dependecy tracking of computed-class, therefore redefining a class so that one of its superclasses is removed
    ;; will not cause this slot to be recomputed. testcase: redefine a class not inheriting from audited-object anymore, export the model, export will error at some point.
    (compute-as (collect-if #'persistent-class-p (class-direct-subclasses -self-)))
    :type (list persistent-class)
    :documentation "The list of persistent direct subclasses.")
   (persistent-effective-subclasses
    (compute-as (compute-persistent-effective-subclasses -self-))
    :type (list persistent-class)
    :documentation "The list of persistent effective subclasses in no particular order.")
   (direct-instances-identity-view
    (compute-as (compute-direct-instances-identity-view -self-))
    :type (or null view)
    :documentation "The view which provides the oid for the direct instances of this class.")
   (direct-instances-prefetch-view
    (compute-as (compute-direct-instances-prefetch-view -self-))
    :type (or null view)
    :documentation "The view which provides the data for the prefetched slots of the direct instances of this class.")
   (direct-instances-data-view
    (compute-as (compute-direct-instances-data-view -self-))
    :type (or null view)
    :documentation "The view which provides the data for the effective slots of the direct instances of this class.")
   (all-instances-identity-view
    (compute-as (compute-all-instances-identity-view -self-))
    :type (or null view)
    :documentation "The view which provides the oid for all instances of this class.")
   (all-instances-prefetch-view
    (compute-as (compute-all-instances-prefetch-view -self-))
    :type (or null view)
    :documentation "The view which provides the data for the prefetched slots of all instances of this class.")
   (all-instances-data-view
    (compute-as (compute-all-instances-data-view -self-))
    :type (or null view)
    :documentation "The view which provides the data for the effective slots of all instances of this class.")
   (primary-table
    (compute-as (compute-primary-table -self- -current-value-))
    :type (or null table)
    :documentation "The table that primarily belongs to this class and will store its direct slots by default. If a class has no slots then the identities of its instances will be stored in its primary table. On the other hand the primary table may or may not store all the effective slots depending on the provided store settings. In general there are no more tables in the RDBMS mapping than the set of all primary tables.")
   (data-tables
    (compute-as (compute-data-tables -self-))
    :type (list table)
    :documentation "All the tables which hold data of the effective slots or the identity of an instance of this class.")
   (data-table-slots
    (compute-as (collect-if #'data-table-slot-p (persistent-effective-slots-of -self-)))
    :type (list persistent-effective-slot-definition)
    :documentation "The list of effective slots which are stored in the data tables, this excludes n-ary association ends.")
   (prefetched-slots
    (compute-as (collect-if #'prefetch-p (persistent-effective-slots-of -self-)))
    :type (list persistent-effective-slot-definition)
    :documentation "The list of effective slots which will be loaded from and stored to the database at once when loading an instance of this class. Moreover when a persistent instance is revived its prefetched slots will be loaded.")
   (non-prefetched-slots
    (compute-as (set-difference (persistent-effective-slots-of -self-) (prefetched-slots-of -self-)))
    :type (list effective-slot)
    :documentation "The list of effective slots which will be loaded and stored lazily and separately from other slots.")
   (depends-on
    (compute-as nil)
    :type (list persistent-class)
    :documentation "The list of persistent classes which must be looked at by this class when computing RDBMS meta data. This is used to generate columns into other classes' primary tables.")
   (depends-on-me
    (compute-as nil)
    :type (list persistent-class)
    :documentation "The list of persistent classes which must look at this class when computing RDBMS meta data."))
  (:documentation "Persistent class is a class meta instance for classes. Standard defclass forms may be used to define persistent classes. A persistent class will have persistent slots unless marked with :persistent #f. A persistent slot should have type specification to be efficient both in storage and speed. The special type unbound must be used to mark slots which might be unbound."))

(def class identity-preserving-class (computed-class)
  ()
  (:documentation "This class serves a very special purpose, namely being able to return the very same instance in make-instance for slot definition meta instances."))

(def computed-class* persistent-slot-definition (standard-slot-definition)
  ;; TODO: make this fallback on sb-pcl::%class on sbcl
  ((%%class :initarg :class :accessor persistent-slot-definition-class) ;; instead of sb-pcl::%class
   (prefetch
    :type boolean
    :computed-in computed-universe/perec
    :documentation "Prefetched slots are loaded from and stored into the database at once. A prefetched slot must be in a table which can be accessed using a where clause matching to the id of the instance thus it must be in a data table. The default prefetched slot semantics can be overriden on a per direct slot basis.")
   (cache
    :type boolean
    :computed-in computed-universe/perec
    :documentation "All prefetched slots are cached slots but the opposite may not be true. When a cached slot is loaded it's value will be stored in the CLOS instance for fast subsequent read operations. Also whenever a cached slot is set the value will be remembered. The default cached slot semantics can be overriden on a per direct slot basis.")
   (index
    :type boolean
    :computed-in computed-universe/perec
    :documentation "True means the slot value will be indexed in the underlying RDBMS.")
   (unique
    :type boolean
    :computed-in computed-universe/perec
    :documentation "True means the slot value will be enforced to be unique among instances in the underlying RDBMS.")
   (specified-type
    (compute-as (compute-specified-type -self-))
    :type (or symbol cons)
    :initarg nil
    :documentation "The slot type as it was specified or t.")
   (canonical-type
    (compute-as (bind (((:values canonical-type error) (ignore-errors
                                                         (canonical-type-for (specified-type-of -self-)))))
                  (or canonical-type
                      (progn
                        ;; due to the MOP we must not fail when this is called, otherwise the entire (sblc) image breaks
                        (warn "Could not process type ~S specified for slot ~S, falling back to type T. The error was: ~A"
                              (specified-type-of -self-) (slot-definition-name -self-) error)
                        (canonical-type-for t)))))
    :type (or symbol cons)
    :documentation "The canonical form of the specified type. See canonical-type-for for more details.")
   (normalized-type
    (compute-as (normalized-type-for (canonical-type-of -self-)))
    :type (or symbol cons)
    :documentation "The normalized form of the specified type. See normalized-type-for for more details.")
   (always-checked-type
    (compute-as (compute-always-checked-type -self-))
    :type t
    :documentation "When type-check is :always then this type will be checked whenever a new value is set during the transaction. This type may be different from the specified type.")
   (type-check
    :type (member :always :on-commit)
    :computed-in computed-universe/perec
    :documentation "On commit type check means that during the transaction the slot may have null and/or unbound value and the type check will be done when the transaction commits."))
  (:documentation "Base class for both persistent direct and effective slot definitions."))

(def computed-class* persistent-direct-slot-definition (persistent-slot-definition standard-direct-slot-definition)
  ((specified-type
    :initarg :type
    :documentation "The slot type as it was originally specified in the defclass form."))
  (:metaclass identity-preserving-class)
  (:documentation "Class for persistent direct slot definitions."))

(def computed-class* persistent-effective-slot-definition (persistent-slot-definition standard-effective-slot-definition)
  ((direct-slots
    :type (list persistent-direct-slot-definition)
    :documentation "The list of direct slots definitions used to compute this effective slot during the class finalization procedure in class precedence list order.")
   (primary-class
    (compute-as (compute-primary-class -self-))
    :type persistent-class
    :documentation "The persistent class which owns the primary table where this slot will be stored, NIL for abstract classes.")
   (table
    (compute-as (compute-table -self-))
    :type table
    :documentation "The RDBMS table which will be queried or updated to get and set the data of this slot, NIL for abstract classes")
   (column-names
    (compute-as (compute-column-names -self-))
    :type list
    :documentation "The list of RDBMS column names to which this slot will be mapped.")
   (columns
    (compute-as (compute-columns -self-))
    :type (list sql-column)
    :documentation "The list of RDBMS columns which will be queried or updated to get and set the data of this slot.")
   (oid-column
    (compute-as (bind ((type (canonical-type-of -self-)))
                  (if (or (persistent-class-type-p* type)
                          (set-type-p* type))
                      (first (columns-of -self-)))))
    :type sql-column
    :documentation "This is the id column of the oid reference when appropriarte for the slot type.")
   (mapping
    (compute-as (compute-slot-mapping -self-))
    :type mapping
    :documentation "The RDBMS mapping")
   (column-types
    (compute-as (awhen (mapping-of -self-) (rdbms-types-of it)))
    :type list
    :documentation "List of RDBMS types to which this slot is mapped.")
   (reader
    (compute-as (compute-slot-reader -self-))
    :type (or null function)
    :documentation "A function which transforms RDBMS values to the corresponding lisp value. This is present only for data table slots.")
   (writer
    (compute-as (compute-slot-writer -self-))
    :type (or null function)
    :documentation "A function which transforms a lisp value to the corresponding RDBMS values. This is present only for data table slots.")
   (primary-table-slot
    (compute-as (compute-primary-table-slot-p -self-))
    :type boolean
    :documentation "True means the slot can be loaded from the primary table of its class with a where clause matching to the instance's oid.")
   (data-table-slot
    (compute-as (compute-data-table-slot-p -self-))
    :type boolean
    :documentation "True means the slot can be loaded from one of the data tables of its class with a where clause matching to the instance's oid.")
   (prefetch
    (compute-as (data-table-slot-p -self-))
    :documentation "The prefetched option is inherited among direct slots according to the class precedence list. If no direct slot has prefetched specification then the default behaviour is to prefetch data tabe slots.")
   (cache
    (compute-as (or (prefetch-p -self-)
                    (persistent-class-type-p* (canonical-type-of -self-))))
    :documentation "The cached option is inherited among direct slots according to the class precedence list. If no direct slot has cached specification then the default behaviour is to cache prefetched slots and single instance references.")
   (index
    (compute-as #f)
    :documentation "The index option is inherited among direct slots according to the class precedence list with defaulting to false.")
   (unique
    (compute-as #f)
    :documentation "The unique option is inherited among direct slots according to the class precedence list with defaulting to false.")
   (specified-type
    :documentation "The types of the direct slots combined with the compound type specifier 'and'.")
   (type-check
    (compute-as (if (persistent-class-type-p* (canonical-type-of -self-))
                    :on-commit
                    :always))
    :documentation "The type check option is inherited among direct slots according to the class precedence list with defaulting to :always. for primitive types and :on-commit for class types.")
   (default-value-for-type
    (compute-as (default-value-for-type (canonical-type-of -self-)))
    :documentation "Computes the default value from the type of the slot. Returns the (DEFAULT-VALUE . HAS-DEFAULT-P) pair."))
  (:documentation "Class for persistent effective slot definitions."))

(eval-always
  (mapc [pushnew !1 *allowed-slot-definition-properties*] '(:persistent :prefetch :cache :index :unique :type-check)))

(def computed-class* class-primary-table (table)
  ((persistent-class
    :type persistent-class
    :documentation "The persistent class for which this table is the primary table.")
   (stored-persistent-classes
    (compute-as (compute-stored-persistent-classes -self-))
    :type (list persistent-class)
    :documentation "The persistent classes which actually store at least one of their effective slots in this table. Abstract classes are not included.")
   (oid-column
    (compute-as (find +oid-column-name+ (columns-of -self-) :key #'hu.dwim.rdbms::name-of :test #'string=))
    :type sql-column
    :documentation "The RDBMS column representing the oid in this table."))
  (:documentation "This is a special table related to a persistent class."))

(def print-object persistent-slot-definition
  (princ (slot-definition-name -self-)))

;;;;;;
;;; defpclass

(def method expand-defpclass-form ((metaclass persistent-class) defclass-macro name superclasses slots options)
  `(,defclass-macro ,name ,superclasses ,slots ,@options))

;;;;;;
;;; Export

(def method export-to-rdbms ((class persistent-class))
  ;; TODO: the view should be first dropped, then the alter statements executed, and after that the view recreated
  ;; TODO: because the view will prevent some alter tables to execute.
  (ensure-finalized class)
  (dolist (superclass (persistent-effective-superclasses-of class))
    (awhen (primary-table-of superclass)
      (ensure-exported it)))
  (awhen (primary-table-of class)
    (ensure-exported it))
  (dolist (subclass (persistent-effective-subclasses-of class))
    (awhen (primary-table-of subclass)
      (ensure-exported it))
    (dolist (subclass-superclass (persistent-effective-superclasses-of subclass))
      (awhen (primary-table-of subclass-superclass)
        (ensure-exported it))))
  (dolist (superclass (list* class (persistent-effective-superclasses-of class)))
    (dolist (association (collect-if (of-type 'persistent-association) (depends-on-of superclass)))
      (ensure-exported association)))
  (awhen (direct-instances-identity-view-of class)
    (ensure-exported it))
  (awhen (direct-instances-prefetch-view-of class)
    (ensure-exported it))
  (awhen (direct-instances-data-view-of class)
    (ensure-exported it))
  (awhen (all-instances-identity-view-of class)
    (ensure-exported it))
  (awhen (all-instances-prefetch-view-of class)
    (ensure-exported it))
  (awhen (all-instances-data-view-of class)
    (ensure-exported it)))

(def function views-of (class)
  (prog1-bind views nil
    (flet ((collect-if-not-nil (view) (when view (push view views))))
      (collect-if-not-nil (direct-instances-identity-view-of class))
      (collect-if-not-nil (direct-instances-prefetch-view-of class))
      (collect-if-not-nil (direct-instances-data-view-of class))
      (collect-if-not-nil (all-instances-identity-view-of class))
      (collect-if-not-nil (all-instances-prefetch-view-of class))
      (collect-if-not-nil (all-instances-data-view-of class)))))

(def function drop-views (views)
  (mapc
   (lambda (view)
     (drop-view (name-of view))
     (invalidate-computed-slot view 'ensure-exported))
   views))

;;;;;;
;;; Mapping

(def method compute-rdbms-types* ((mapped-type persistent-class) normalized-type)
  (compute-rdbms-types* (class-name mapped-type) normalized-type))

(def method compute-reader* ((mapped-type persistent-class) normalized-type)
  (compute-reader* (class-name mapped-type) normalized-type))

(def method compute-writer* ((mapped-type persistent-class) normalized-type)
  (compute-writer* (class-name mapped-type) normalized-type))

;;;;;;
;;; Computed

(def generic compute-class-id (class)
  (:method ((class persistent-class))
    (bind ((class-name-bytes (string-to-octets (symbol-name (class-name class)) :encoding :utf-8)))
      ;; TODO FIXME the probability of clashes is too high this way. e.g. see failing test test/persistence/export/class-id/bug1
      (mod (ironclad:octets-to-integer (ironclad:digest-sequence :crc32 class-name-bytes))
           +oid-maximum-class-id+))))

(def generic compute-always-checked-type (slot)
  (:method ((slot persistent-slot-definition))
    (bind ((type (canonical-type-of slot)))
      (if (and (eq :on-commit (type-check-of slot))
               (not (slot-definition-initfunction slot)))
          `(or unbound ,type)
          type))))

(def generic compute-specified-type (slot)
  (:method ((slot persistent-direct-slot-definition))
    t)

  (:method ((slot persistent-effective-slot-definition))
    (bind ((types (remove t (mapcar (lambda (direct-slot)
                                      (etypecase direct-slot
                                        (persistent-effective-slot-definition (specified-type-of direct-slot))
                                        (standard-slot-definition (slot-definition-type direct-slot))))
                                    (direct-slots-of slot)))))
      (if (length= 1 types)
          (first types)
          (cons 'and types)))))

(def generic compute-persistent-effective-superclasses (class)
  (:method ((class persistent-class))
    (ensure-finalized class)
    (cdr (collect-if (of-type 'persistent-class)
                     (class-precedence-list class)))))

(def generic compute-persistent-effective-subclasses (class)
  (:method ((class persistent-class))
    (remove-duplicates
     (append (persistent-direct-subclasses-of class)
             (iter (for subclass in (persistent-direct-subclasses-of class))
                   (appending (persistent-effective-subclasses-of subclass)))))))

(def generic compute-stored-persistent-classes (table)
  (:method ((table class-primary-table))
    (bind ((class (persistent-class-of table)))
      (collect-if [and (not (abstract-p !1)) (member table (data-tables-of !1))]
                  (append (persistent-effective-superclasses-of class)
                          (list class)
                          (persistent-effective-subclasses-of class))))))

(def generic compute-effective-store (class)
  (:method ((class persistent-class))
    (bind ((seen-classes nil)
           (class-precedence-list (persistent-class-precedence-list-of class)))
      (iter (for superclass :in class-precedence-list)
            (labels ((find-primary-class (class)
                       (assert (not (member class seen-classes)) nil "Circularity in store definitions of ~A" seen-classes)
                       (bind ((direct-store (direct-store-of class))
                              (option (or (second (find (class-name superclass) direct-store
                                                        :key (lambda (element)
                                                               (if (consp element)
                                                                   (first element)
                                                                   element))))
                                          (find-if #'symbolp direct-store)))
                              (position (position class class-precedence-list)))
                         (push class seen-classes)
                         (prog1
                             (case option
                               (:push-up (iter (for less-specific-class :in (subseq class-precedence-list (1+ position)))
                                               (when (subtypep less-specific-class superclass)
                                                 (return (find-primary-class less-specific-class)))))
                               (:push-down (iter (for less-specific-class :in (nreverse (subseq class-precedence-list 0 position)))
                                                 (when (subtypep less-specific-class superclass)
                                                   (return (find-primary-class less-specific-class)))))
                               (:separate class)
                               ((nil) class)
                               (t (find-class option)))
                           (pop seen-classes)))))
              (bind ((primary-class (find-primary-class superclass)))
                (when primary-class
                  (collect (list (class-name superclass)
                                 (class-name primary-class))))))))))

(def generic compute-slot-mapping (slot)
  (:method ((slot persistent-effective-slot-definition))
    (compute-mapping (always-checked-type-of slot))))

(def generic compute-slot-reader (slot)
  (:method ((slot persistent-effective-slot-definition))
    (coerce (reader-of (mapping-of slot)) 'function)) )

(def generic compute-slot-writer (slot)
  (:method ((slot persistent-effective-slot-definition))
    (coerce (writer-of (mapping-of slot)) 'function)))

(def generic compute-primary-table (class current-table)
  (:method ((class persistent-class) current-table)
    (ensure-finalized class)
    (flet ((compute-columns ()
             (nreverse
              (prog1-bind columns nil
                (flet ((push-columns (related-class)
                         (unless (abstract-p related-class)
                           (ensure-finalized related-class)
                           (dolist (slot (persistent-effective-slots-of related-class))
                             (when (eq (primary-class-of slot) class)
                               (dolist (column (columns-of slot))
                                 (bind ((found-column (find (hu.dwim.rdbms::name-of column) columns :key #'hu.dwim.rdbms::name-of :test #'string=)))
                                   (assert (or (not found-column)
                                               (column-equal-p column found-column))
                                           nil "Different columns with the same name ~A, ~A~%while building the primary table for class ~A"
                                           column found-column class)
                                   (unless found-column
                                     (pushnew column columns)))))))))
                  (map nil #'push-columns (persistent-effective-superclasses-of class))
                  (map nil #'push-columns (persistent-effective-subclasses-of class))
                  (push-columns class)
                  (map nil #'push-columns (collect-if (of-type 'persistent-class) (depends-on-of class))))))))
      (when (or (not (abstract-p class))
                (eq (class-name class) (find-class-store-location class class))
                (compute-columns))
        (or current-table
            (make-instance 'class-primary-table
                           :name (rdbms-name-for (class-name class) :table)
                           :persistent-class class
                           :columns (compute-as
                                      (append
                                       (list (make-oid-column))
                                       (compute-columns)))))))))

(def generic compute-direct-instances-identity-view (class)
  (:method ((class persistent-class))
    (make-view-for-classes-and-slots (view-name-for-class class "_di")
                                     (list class)
                                     nil)))

(def generic compute-direct-instances-prefetch-view (class)
  (:method ((class persistent-class))
    (make-view-for-classes-and-slots (view-name-for-class class "_dp")
                                     (list class)
                                     (mapcar #'slot-definition-name (prefetched-slots-of class)))))

(def generic compute-direct-instances-data-view (class)
  (:method ((class persistent-class))
    (make-view-for-classes-and-slots (view-name-for-class class "_dd")
                                     (list class)
                                     (mapcar #'slot-definition-name (data-table-slots-of class)))))

(def generic compute-all-instances-identity-view (class)
  (:method ((class persistent-class))
    (make-view-for-classes-and-slots (view-name-for-class class "_ai")
                                     (list* class (persistent-effective-subclasses-of class))
                                     nil)))

(def generic compute-all-instances-prefetch-view (class)
  (:method ((class persistent-class))
    (make-view-for-classes-and-slots (view-name-for-class class "_ap")
                                     (list* class (persistent-effective-subclasses-of class))
                                     (mapcar #'slot-definition-name (prefetched-slots-of class)))))

(def generic compute-all-instances-data-view (class)
  (:method ((class persistent-class))
    (make-view-for-classes-and-slots (view-name-for-class class "_ad")
                                     (list* class (persistent-effective-subclasses-of class))
                                     (mapcar #'slot-definition-name (data-table-slots-of class)))))

(def generic compute-data-tables (class)
  (:method ((class persistent-class))
    (remove nil
            (remove-duplicates
             (append (mapcar #'table-of (data-table-slots-of class))
                     (mapcar [awhen (find-class-store-location class !1) (primary-table-of (find-class it))]
                             (list* class (persistent-effective-superclasses-of class))))))))

(def generic compute-primary-table-slot-p (slot)
  (:method ((slot persistent-effective-slot-definition))
    (and (data-table-slot-p slot)
         (eq (primary-class-of slot) (persistent-slot-definition-class slot)))))

(def generic compute-data-table-slot-p (slot)
  (:method ((slot persistent-effective-slot-definition))
    (bind ((type (canonical-type-of slot)))
      (or (primitive-type-p* type)
          (persistent-class-type-p* type)))))

(def generic compute-primary-class (slot)
  (:method ((slot persistent-effective-slot-definition))
    (bind ((type (canonical-type-of slot))
           (owner-class (persistent-slot-definition-class slot))
           (slot-definer-superclass (slot-definer-superclass slot)))
      (awhen (if (set-type-p* type)
                 (bind ((referred-class (find-class (set-type-class-for type))))
                   (find-class-store-location referred-class referred-class))
                 (find-class-store-location owner-class slot-definer-superclass))
        (find-class it)))))

(def generic compute-table (slot)
  (:method ((slot persistent-effective-slot-definition))
    (awhen (primary-class-of slot)
      (primary-table-of it))))

(def generic compute-column-names (slot)
  (:method ((slot persistent-effective-slot-definition))
    (mapcar #'hu.dwim.rdbms::name-of (columns-of slot))))

(def generic compute-columns (slot)
  (:method ((slot persistent-effective-slot-definition))
    (bind ((class (persistent-slot-definition-class slot))
           (primary-class (primary-class-of slot))
           (class-name (class-name (or primary-class class)))
           (name (slot-definition-name slot))
           (type (canonical-type-of slot))
           (mapping (mapping-of slot))
           (rdbms-types (column-types-of slot)))
      (when type
        (cond ((set-type-p* type)
               (list (make-column-for-reference-slot class-name (string+ (symbol-name name) "-for-" (symbol-name class-name)))))
              ((persistent-class-type-p* type)
               (append
                (when (tagged-p mapping)
                  (list (make-tag-column mapping name)))
                (list (make-column-for-reference-slot class-name name))))
              ((primitive-type-p* type)
               (append
                (when (tagged-p mapping)
                  (list (make-tag-column mapping name)))
                (list
                 (make-instance 'column
                                :name (rdbms-name-for name :column)
                                :type (if (tagged-p mapping)
                                          (second rdbms-types)
                                          (first rdbms-types))
                                ;; TODO: add null constraint if type-check is :always (and (not (subytpep 'null type))
                                ;;                                                         (not (subytpep 'unbound type)))
                                :constraints (if (unique-p slot)
                                                 (list (sql-unique-constraint)))
                                :index (if (and (index-p slot)
                                                (not (unique-p slot)))
                                           (sql-index :name
                                                      (rdbms-name-for (concatenate-symbol name "-on-" class-name "-idx")
                                                                      :index)))))))
              (t
               (error "Unknown type ~A in slot ~A" type slot)))))))

;;;;;;
;;; Utility

;; TODO reloading perec shouldn't clear this state
(def (special-variable :documentation "A mapping from persistent class names to persistent instances.")
    *persistent-classes* (make-hash-table))

(def (function e) find-persistent-class (name)
  (gethash name *persistent-classes*))

(def function find-persistent-class* (name-or-class)
  (etypecase name-or-class
    (symbol (find-persistent-class name-or-class))
    (persistent-class name-or-class)))

(def function (setf find-persistent-class) (new-value name)
  (setf (gethash name *persistent-classes*) new-value))

(def function finalize-persistent-classes ()
  (iter (for (class-name class) :in-hashtable *persistent-classes*)
        (ensure-all-computed-slots-are-valid class)
        (dolist (slot (class-slots class))
          (ensure-all-computed-slots-are-valid slot))))

(def function persistent-class-p (class)
  (typep class 'persistent-class))

(def function persistent-class-name-p (name)
  (and name
       (symbolp name)
       (aif (find-class name #f)
            (persistent-class-p it)
            ;; FIXME gives false positives (for standard classes)
            (forthcoming-defclass-type-p name))))

(def (function io) forthcoming-defclass-type-p (class-name)
  #+sbcl(eq (sb-int:info :type :kind class-name) :forthcoming-defclass-type)
  #-sbcl(not-yet-implemented))

(def function persistent-slot-p (slot)
  (typep slot 'persistent-slot-definition))

(def function find-persistent-slot (class-or-name slot-name &key (otherwise :error otherwise?))
  (or (find slot-name
            (the list
              (persistent-effective-slots-of (if (symbolp class-or-name)
                                                 (find-class class-or-name)
                                                 class-or-name)))
            :key #'slot-definition-name
            :test #'eq)
      (handle-otherwise (error "Could not find the persistent slot ~S of class ~A" slot-name class-or-name))))

(def function persistent-effective-slot-precedence-list-of (slot)
  (bind ((slot-name (slot-definition-name slot))
         (slot-class (persistent-slot-definition-class slot)))
    (ensure-finalized slot-class)
    (iter (for class in (persistent-effective-superclasses-of slot-class))
          (ensure-finalized class)
          (aif (find slot-name (persistent-effective-slots-of class) :key #'slot-definition-name)
               (collect it)))))

(def function slot-accessor-p (name)
  (and (symbolp name)
       (effective-slots-for-accessor name)))

(def function effective-slots-for-accessor (name)
  (iter (for (class-name class) in-hashtable *persistent-classes*)
        (awhen (find name (persistent-direct-slots-of class)
                     :key #'slot-definition-readers
                     :test #'member)
          (ensure-finalized class)
          (collect (prog1 (find-slot class (slot-definition-name it))
                     (assert it))))))

(def function persistent-effective-slots-for-slot-name (slot-name)
  (iter (for (class-name class) in-hashtable *persistent-classes*)
        (for slot = (find-slot (ensure-finalized class) slot-name :otherwise nil))
        (when (typep slot 'persistent-effective-slot-definition)
          (collect slot))))

(def function slot-definer-superclass (slot)
  (persistent-slot-definition-class (find-if (of-type 'persistent-direct-slot-definition) (direct-slots-of slot) :from-end #t)))

(def function find-class-store-location (owner-class definer-class)
  (second (find (class-name definer-class) (effective-store-of owner-class) :key #'first)))

(def function view-name-for-class (class suffix)
  (rdbms-name-for (string+ (symbol-name (class-name class)) suffix) :view))

(def function make-oid-column ()
  "Creates an RDBMS column that will be used to store the oid of the instances in this table."
  (make-instance 'column
                 :name +oid-column-name+
                 :type +oid-sql-type+
                 :constraints (list (sql-not-null-constraint)
                                    (sql-primary-key-constraint))))

(def function make-column-for-reference-slot (class-name column-name)
  (bind ((oid-column-name (rdbms-name-for (concatenate-symbol column-name "-oid") :column))
         (oid-index-name (rdbms-name-for (concatenate-symbol column-name "-oid-on-" class-name "-idx") :index)))
    (make-instance 'column
                   :name oid-column-name
                   :type +oid-sql-type+
                   :index (sql-index :name oid-index-name))))

(def function make-tag-column (mapping name)
  (bind ((type (first (rdbms-types-of mapping)))
         (nullable (first (nullable-types-of mapping))))
    (make-instance 'column
                   :name (rdbms-name-for (concatenate-symbol name "-tag") :column)
                   :type type
                   :constraints (unless nullable
                                  (list (sql-not-null-constraint)))
                   :default-value (if nullable
                                      :null
                                      0))))

(def function make-class-id-matcher-where-clause (classes)
  (bind ((oid-clause (sql-binary-operator :name "&"
                                          :left (sql-identifier :name +oid-column-name+)
                                          :right +oid-maximum-class-id+)))
    (if (length= 1 classes)
        (sql-= oid-clause (id-of (first classes)))
        (sql-in oid-clause (mapcar #'id-of classes)))))

(def (function e) make-view-for-classes-and-slots (name class-or-names slot-names)
  (when-bind query (make-query-for-classes-and-slots class-or-names slot-names)
    (make-instance 'view
                   :name name
                   :columns nil
                   :query query)))

(def method matches-type* (value (type symbol))
  (and (typep value type)
       (or (not (persistent-class-type-p type))
           (every (lambda (slot)
                    (bind ((type (canonical-type-of slot))
                           (class (class-of value)))
                      (unless (funcall *matches-type-cut-function* value type)
                        (if (slot-boundp-using-class class value slot)
                            (bind ((slot-value (slot-value-using-class class value slot)))
                              (aprog1 (matches-type* slot-value type)
                                (unless it
                                  (error (make-condition 'instance-slot-type-violation :instance value :slot slot)))))
                            (not (unbound-subtype-p type))))))
                  (persistent-effective-slots-of type)))))

;;;;;;
;;; Building simple queries

(def class* storage-location ()
  ((tables :type list)
   (classes :type list)
   (slot-names :type list)
   (where :type (or null sql-syntax-node))
   (need-where-clause :type boolean)))

(def constructor storage-location ()
  (assert (every (of-type 'class-primary-table) (tables-of -self-)))
  (assert (every (of-type 'persistent-class) (classes-of -self-)))
  (assert (every #'symbolp (slot-names-of -self-))))

(def function find-and-ensure-classes (classes-or-class-names)
  (mapcar (lambda (class)
            (ensure-finalized (if (symbolp class)
                                  (find-class class)
                                  class)))
          classes-or-class-names))

(def function make-hash-table-from-list (list key-function)
  (aprog1 (make-hash-table :test #'equal)
    (iter (for element :in list)
          (push element (gethash (funcall key-function element) it)))))

(def function make-list-from-hash-table (hash-table element-function)
  (iter (for (key value) :in-hashtable hash-table)
        (collect (funcall element-function key value))))

(def function compute-need-where-clause (storage-location)
  (set-difference (reduce #'intersection (mapcar #'stored-persistent-classes-of (tables-of storage-location)))
                  (classes-of storage-location)))

(def function update-need-where-clauses (storage-locations)
  (map nil [setf (need-where-clause-p !1) (compute-need-where-clause !1)] storage-locations))

(def function update-storage-location-where-clauses (storage-locations)
  (iter (for storage-location :in storage-locations)
        (setf (where-of storage-location)
              (when (compute-need-where-clause storage-location)
                (make-class-id-matcher-where-clause (classes-of storage-location)))))
  storage-locations)

(def function update-storage-location-tables (storage-locations)
  (iter (for storage-location :in storage-locations)
        (for classes = (classes-of storage-location))
        (when (and (length= 1 classes)
                   (compute-need-where-clause storage-location))
          (when-bind table
              (find-if [equal classes (stored-persistent-classes-of !1)]
                       (data-tables-of (first classes)))
            (push table (tables-of storage-location)))))
  storage-locations)

(def function merge-storage-location-classes (storage-locations)
  (make-list-from-hash-table (make-hash-table-from-list storage-locations [list (tables-of !1) (slot-names-of !1)])
                             (lambda (key value)
                               (make-instance 'storage-location
                                              :tables (first key)
                                              :slot-names (second key)
                                              :classes (mappend #'classes-of value)))))

(def function merge-storage-location-slot-names (storage-locations)
  (make-list-from-hash-table (make-hash-table-from-list storage-locations [list (tables-of !1) (classes-of !1)])
                             (lambda (key value)
                               (make-instance 'storage-location
                                              :tables (first key)
                                              :slot-names (mappend #'slot-names-of value)
                                              :classes (second key)))))

(def function merge-covering-storage-locations (storage-locations)
  (update-need-where-clauses storage-locations)
  (tagbody
   :restart
     (iter (for storage-location :in storage-locations)
           (for classes = (classes-of storage-location))
           (for need-where-clause? = (need-where-clause-p storage-location))
           (when (find-if (lambda (cover-storage-location)
                            (bind ((cover-classes (classes-of cover-storage-location))
                                   (cover-need-where-clause? (need-where-clause-p cover-storage-location)))
                              (and (not (eq storage-location
                                            cover-storage-location))
                                   (if (set-equal classes cover-classes)
                                       (and need-where-clause?
                                            (not cover-need-where-clause?))
                                       (subsetp classes cover-classes)))))
                          storage-locations)
             (removef storage-locations storage-location)
             (go :restart))))
  storage-locations)

(def (function e) collect-storage-locations-for-updating-classes-and-slots (classes-or-class-names slot-names)
  (update-storage-location-where-clauses
   (merge-storage-location-slot-names
    (merge-storage-location-classes
     (iter outer
           (for class :in (remove-if #'abstract-p (find-and-ensure-classes classes-or-class-names)))
           (iter (for slot-name :in slot-names)
                 (for slot = (find-slot class slot-name))
                 (in outer (collect (make-instance 'storage-location
                                                   :tables (list (table-of slot))
                                                   :classes (list class)
                                                   :slot-names (list slot-name))))))))))

(def (function e) collect-storage-locations-for-selecting-classes-and-slots (classes-or-class-names slot-names)
  (update-storage-location-where-clauses
   (update-storage-location-tables
    (funcall (if slot-names
                 #'identity
                 #'merge-covering-storage-locations)
             (merge-storage-location-classes
              (iter outer
                    (for class :in (remove-if #'abstract-p (find-and-ensure-classes classes-or-class-names)))
                    (if slot-names
                        (collect (make-instance 'storage-location
                                                :tables (delete-duplicates (mapcar [the (not null) (table-of (find-slot class !1))] slot-names))
                                                :classes (list class)
                                                :slot-names slot-names))
                        (iter (for table :in (data-tables-of class))
                              (in outer (collect (make-instance 'storage-location
                                                                :tables (list (the (not null) table))
                                                                :classes (list class)
                                                                :slot-names nil)))))))))))

(def (function e) make-query-for-classes-and-slots (classes-or-class-names &optional slot-names)
  (first
   (reduce (lambda (accumulator storage-location)
             (bind ((classes (classes-of storage-location))
                    (subquery (sql-select :columns (list* +oid-column-name+
                                                          (mappend (lambda (slot-name)
                                                                     (bind ((slot (find-slot (first classes) slot-name))
                                                                            (table-name (name-of (table-of slot))))
                                                                       (mapcar [sql-column-alias :column !1 :table table-name]
                                                                               (column-names-of slot))))
                                                                   (slot-names-of storage-location)))
                                          :tables (list (reduce [sql-joined-table :kind :inner :using +oid-column-names+ :left !1 :right !2]
                                                                (mapcar [sql-identifier :name (name-of !1)] (tables-of storage-location))))
                                          :where (where-of storage-location))))
               (if accumulator
                   (bind ((subquery-1 (first accumulator))
                          (subquery-2 subquery)
                          (covered-classes-1 (second accumulator))
                          (covered-classes-2 (classes-of storage-location))
                          (all? (not (intersection covered-classes-1 covered-classes-2))))
                     (list (if (and (typep subquery-1 'sql-set-operation-expression)
                                    (eq all? (hu.dwim.rdbms::all-p subquery-1)))
                               (progn
                                 (push subquery-2 (hu.dwim.rdbms::subqueries-of subquery-1))
                                 subquery-1)
                               (sql-set-operation-expression :set-operation :union
                                                             :all all?
                                                             :subqueries (list subquery-1 subquery-2)))
                           (union covered-classes-1 covered-classes-2)))
                   (list subquery
                         classes))))
           (collect-storage-locations-for-selecting-classes-and-slots classes-or-class-names slot-names)
           :initial-value nil)))

(def (function e) collect-all-persistent-class-tables ()
  (sort (remove nil (mapcar 'primary-table-of (hash-table-values *persistent-classes*)))
        #'string< :key #'name-of))
