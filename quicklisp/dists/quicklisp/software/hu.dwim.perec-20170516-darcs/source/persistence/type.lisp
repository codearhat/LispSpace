;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;;;
;;; Defining types

(def constant +type-error-marker+ '+type-error-marker+)

(def constant +ignore-in-rdbms-equality-marker+ '+ignore-in-rdbms-equality-marker+)

(def special-variable *persistent-types* (make-hash-table))

(def class* persistent-type ()
  ((name
    :type symbol)
   (documentation
    :type (or null string))
   (args
    :type list)
   (body
    :type list)
   (substituter
    :type function)))

(def (definer e :available-flags "e") persistent-type (name args &body body)
  (bind ((type-class-name (type-class-name-for name)))
    `(progn
       (defptype ,name ,args
         ,@body)
       ,@(when (getf -options- :export)
          `((eval-when (:compile-toplevel :load-toplevel :execute)
              ;; this import is needed because the name of the type class for types named by cl symbols are interned into
              ;; the hu.dwim.perec package and therefore cannot be exported from *package*. make sure it's imported.
              (import ',type-class-name ,*package*)
              (export '(,name ,type-class-name) ,*package*)))))))

;; TODO: this persistent-type stuff is superfluous as soon as there's a portable? typeexpand-1
;;       the generated type class may be not needed too, see where parse-type is used
(def macro defptype (name args &body body)
  (bind ((common-lisp-type-p (eq (symbol-package name) #.(find-package :common-lisp)))
         (allow-nil-args-p (or (null args)
                               (member (first args) '(&rest &optional))))
         (documentation (when (stringp (first body))
                          (pop body)))
         (type-class-name (type-class-name-for name)))
    `(progn
      (defclass* ,type-class-name (persistent-type)
        ,(append
          `((name ',name)
            (args ',args)
            (body ',body)
            (substituter
             (named-lambda ,(format-symbol *package* "TYPE-SUBSTITUTER/~A" name) ,args
               ,@body)
             :allocation :class)
            (parser
             (named-lambda ,(format-symbol *package* "TYPE-PARSER/~A" name) ,args
               (make-instance ',type-class-name
                              ,@(mappend (lambda (arg)
                                           (list (intern (symbol-name arg) #.(find-package :keyword))
                                                 `(if (and (symbolp ,arg)
                                                           (constantp ,arg))
                                                      (symbol-value ,arg)
                                                      ,arg)))
                                         (lambda-list-to-variable-list args :include-&rest #t))))
             :allocation :class))
          (mapcar [list !1 nil] (lambda-list-to-variable-list args :include-&rest #t)))
        (:export-accessor-names-p #t))
      (eval-when (:load-toplevel :execute)
        (bind ((class (ensure-finalized (find-class ',type-class-name))))
          (declare (ignorable class))
          ,(when allow-nil-args-p
                 `(bind ((prototype (class-prototype class))
                         (substituter (substituter-of prototype))
                         (substituted-type (funcall substituter)))
                   (ensure-class ',type-class-name :direct-superclasses
                    (list (ensure-finalized (find-class (type-superclass-name-for ',name substituted-type)))))
                   (bind ((type (if (symbolp substituted-type)
                                    (make-instance ',type-class-name)
                                    (change-class (parse-type substituted-type) ',type-class-name))))
                     (setf (name-of type) ',name)
                     (setf (args-of type) ',args)
                     (setf (body-of type) ',body)
                     (setf (documentation-of type) ',documentation)
                     (setf (find-type ',name) type))))))
      ,(if (or common-lisp-type-p
               (and (find-class name #f)
                    (progn
                      (simple-style-warning "Defining a persistent type named ~S, but it already names a class. Skipping the DEFTYPE form..." name)
                      #t)))
           `',name
           `(deftype ,name ,args ,@body)))))

(def (function o) find-type (type &key (otherwise :error))
  (or (gethash (first (ensure-list type)) *persistent-types*)
      (case otherwise
        (:error (error "Unknown type specifier ~S" type))
        (:warn (warn "Unknown type specifier ~S" type)
               nil)
        (t otherwise))))

(def (function o) (setf find-type) (new-value type)
  (setf (gethash (first (ensure-list type)) *persistent-types*) new-value))

(def function type-class-name-for (type)
  (concatenate-symbol (if (eq (symbol-package type)
                              (find-package :common-lisp))
                          (find-package :hu.dwim.perec)
                          (symbol-package type))
                      type "-type"))

(def function type-superclass-name-for (name type)
  (cond ((and (symbolp type)
              (not (eq name type)))
         (if (find-class (type-class-name-for type) nil)
             (type-class-name-for type)
             'persistent-type))
        ((and (consp type)
              (not (eq name (first type))))
         (let ((el (first type)))
           (cond ((eq 'and el)
                  (let ((superclass-name (type-class-name-for (find-if #'symbolp (cdr type)))))
                    (if (find-class superclass-name nil)
                        superclass-name
                        (type-class-name-for el))))
                 ((member el '(or not))
                  'persistent-type)
                 (t
                  (type-class-name-for el)))))
        (t
         'persistent-type)))

(def function type-specifier-p (type)
  (find-type type :otherwise #f))

(def function substitute-type-arguments (type args)
  (apply (substituter-of (find-type type)) args))

;;;;;;
;;; Type checker

(def special-variable *type-check-slot-values* #t)

(def (macro e) with-type-checking-slot-values (&body body)
  `(bind ((*type-check-slot-values* #t))
    ,@body))

(def (macro e) without-type-checking-slot-values (&body body)
  `(bind ((*type-check-slot-values* #f))
    ,@body))

(def condition* slot-type-error (type-error)
  ((instance
    nil
    :type persistent-object)
   (slot
    :type persistent-effective-slot-definition))
  (:report
   (lambda (condition stream)
     (format stream
             "~@<The value ~2I~:_~S ~I~_in slot ~A of instance ~A is not of type ~2I~_~S.~:>"
             (type-error-datum condition)
             (slot-definition-name (slot-of condition))
             (instance-of condition)
             (type-error-expected-type condition)))))

;; TODO: take care about performance
;; TODO: when type-check is :on-commit checks during the transaction should be debug-only and at the end do the real type-check
(def (function o) check-slot-value-type (instance slot slot-value &optional (on-commit #f))
  (when *type-check-slot-values*
    (bind ((canonical-type (canonical-type-of slot))
           (type (if on-commit
                     canonical-type
                     (always-checked-type-of slot)))
           (expected-type (if (set-type-p* canonical-type)
                              (set-type-class-for canonical-type)
                              type)))
      (unless (typep slot-value expected-type)
        (cerror "Ignore type error and mark transaction for rollback only" 'slot-type-error :instance instance :slot slot :datum slot-value :expected-type type)
        ;; TODO this is not doing what seems to be obviously doing: MARK-TRANSACTION-FOR-ROLLBACK-ONLY is too late here... rdbms should assert for this, TODO marked over there...
        (mark-transaction-for-rollback-only)
        #t))))

;;;;;;
;;; Canonical type

(def function canonical-type-for (type)
  (iter (for simplified-type :initially type :then (simplify-boolean-form (canonical-type-for* (->dnf simplified-type))))
        (for previous-type :previous simplified-type)
        (until (equal simplified-type previous-type))
        (finally (return simplified-type))))

(def (special-variable :documentation "A list of type names to be treated as canonical types when a type is converted into canonical form.")
    *canonical-types*
    '(nil
      unbound
      null
      boolean
      integer
      float
      float-32
      float-64
      double
      number
      text
      duration
      string
      timestamp
      date
      time-of-day
      ip-address-vector
      unsigned-byte-vector
      member
      symbol*
      symbol
      form
      serialized
      set
      disjunct-set
      ordered-set
      t))

(def function find-class* (class-or-name)
  (if (typep class-or-name 'standard-class)
      class-or-name
      (find-class class-or-name)))

(def function canonical-type-p (type)
  (to-boolean (member (first (ensure-list type)) *canonical-types*)))

(def function class-type-p (type)
  (and (symbolp type)
       (find-class type nil)))

(def function disjunct-type-p (type-1 type-2)
  (equal '(#t #t)
         (multiple-value-list
          (subtypep `(and ,type-1 ,type-2) nil))))

(def function disjunct-type-p* (type-1 type-2)
  (equal '(#t #t)
         (multiple-value-list
          (subtypep (canonical-type-for `(and ,type-1 ,type-2)) nil))))

(def function canonical-type-for* (type)
  ;; TODO use lisp constructs for the non-pattern-matching part like canonical-type-p, etc
  (pattern-case type
    ((?is ?type canonical-type-p)
     type)
    ;; simple class
    ((?is ?type class-type-p)
     type)
    ;; set types
    ((?is ?type set-type-p*)
     type)
    ;; (and a (not a)) -> nil
    ((?or (and (?* ?x) ?a (?* ?y) (not ?a) (?* ?z))
          (and (?* ?x) (not ?a) (?* ?y) ?a (?* ?z)))
     nil)
    ;; (and a a) -> a
    ((and (?* ?x) ?a (?* ?y) ?a (?* ?z))
     (canonical-type-for (list* 'and (append ?x (list ?a) ?y ?z))))
    ;; (or a (not a)) -> t
    ((?or (or (?* ?x) ?a (?* ?y) (not ?a) (?* ?z))
          (or (?* ?x) (not ?a) (?* ?y) ?a (?* ?z)))
     t)
    ;; (or a a) -> a
    ((or (?* ?x) ?a (?* ?y) ?a (?* ?z))
     (canonical-type-for (list* 'or (append ?x (list ?a) ?y ?z))))
    ;; persistent classes without intersecion
    ((and (?* ?x) ?a (?* ?x) ?b (?* ?z)
          (?if (and (persistent-class-type-p ?a)
                    (persistent-class-type-p ?b)
                    (not (intersection (list* (find-class* ?a) (persistent-effective-subclasses-of (find-class* ?a)))
                                       (list* (find-class* ?b) (persistent-effective-subclasses-of (find-class* ?b))))))))
     nil)
    ;; (disjunct-type-p a b) -> nil
    ((?or (and (?* ?x) ?a (?* ?y) ?b (?* ?z)
               (?if (disjunct-type-p ?a ?b)))
          (and (?* ?x) ?b (?* ?y) ?a (?* ?z)
               (?if (disjunct-type-p ?a ?b))))
     nil)
    ;; (disjunct-type-p a (not b)) -> a
    ((?or (and (?* ?x) ?a (?* ?y) (not ?b) (?* ?z)
               (?if (or (disjunct-type-p ?a ?b)
                        (disjunct-type-p* ?a ?b))))
          (and (?* ?x) (not ?b) (?* ?y) ?a (?* ?z)
               (?if (or (disjunct-type-p ?a ?b)
                        (disjunct-type-p* ?a ?b))))
          ;; (subtypep a b) -> a
          (and (?* ?x) ?a (?* ?y) ?b (?* ?z)
               (?if (subtypep ?a ?b)))
          (and (?* ?x) ?b (?* ?y) ?a (?* ?z)
               (?if (subtypep ?a ?b))))
     (canonical-type-for `(and ,@?x ,@?y ,?a ,@?z)))
    ;; recursion for and/or arguments
    (((?or or and not) . ?args)
     (list* (first type) (mapcar #'canonical-type-for (cdr type))))
    ;; expand types
    ((?is ?type symbolp)
     (canonical-type-for (substitute-type-arguments type nil)))
    ;; known system type
    ((?is ?type type-specifier-p)
     (let ((substituted-type (substitute-type-arguments (first type) (cdr type))))
       (if (not (equal substituted-type type))
           (canonical-type-for substituted-type)
           type)))
    ;; no more options
    (?type
     type)))

;;;;;;
;;; Type mapping

(def (special-variable :documentation "An ordered list of types which are mapped to RDBMS.") *mapped-type-precedence-list*
    '(nil
      unbound
      null
      boolean
      integer-8
      integer-16
      integer-32
      integer-64
      integer
      float-32
      float-64
      float
      double
      duration
      number
      text
      string
      date
      time-of-day
      timestamp
      ip-address-vector
      unsigned-byte-vector
      member
      symbol*
      symbol
      form
      serialized
      set
      disjunct-set
      ordered-set
      t))

(def class* mapping ()
  ((specified-type
    :type (or symbol list)
    :documentation "The original mapped lisp type.")
   (reader
    :type (or symbol function)
    :documentation "A function which is used to transform a sequence of RDBMS values to a lisp value.")
   (writer
    :type (or symbol function)
    :documentation "A function which is used to transform a lisp value to a sequence of RDBMS values.")
   (unit-types
    :type list
    :documentation "The list of mapped unit types (having exactly one value) which are subtypes of the specified type.")
   (tagged
    :type boolean
    :documentation "Tagged means different subtypes are differentiated by an extra tag column. In this case the first RDBMS-TYPES corresponds to the tag column.")
   (rdbms-types
    :type list
    :documentation "A list of RDBMS types used to store lisp values")
   (nullable-types
    :type list))
  (:documentation "Describes hte mapping of a lisp type to RDBMS."))

;; TODO use -normalized-type- naming convention, export stuff
(def macro defmapping (name sql-type reader writer)
  "A mapping specifies how a type is mapped to RDBMS. It defines the transformers to convert between the rdbms values and the slot value."
  (flet ((function-designator-p (transformer)
           (and (consp transformer)
                (eq (first transformer) 'quote)
                (symbolp (second transformer))
                (second transformer))))
    `(progn
       (def method compute-rdbms-types* ((mapped-type (eql ',name)) normalized-type)
         (declare (ignorable normalized-type))
         (ensure-list ,sql-type))

       (def method compute-reader* ((mapped-type (eql ',name)) normalized-type)
         (declare (ignorable normalized-type))
         ,(aif (function-designator-p reader)
               (if *load-as-production?*
                   `(fdefinition ',it)
                   `',it)
               reader))

       (def method compute-writer* ((mapped-type (eql ',name)) normalized-type)
         (declare (ignorable normalized-type))
         ,(aif (function-designator-p writer)
               (if *load-as-production?*
                   `(fdefinition ',it)
                   `',it)
               writer)))))

(def generic compute-type-tag (mapped-type)
  (:documentation "Returns a type tag which will be stored in the tag column when needed.")

  (:method (mapped-type)
    0))

(def function compute-rdbms-types (type)
  (rdbms-types-of (compute-mapping type)))

(def generic compute-rdbms-types* (mapped-type normalized-type)
  (:method (mapped-type normalized-type)
    (error "Cannot map type ~A to RDBMS types" mapped-type))

  (:method ((mapped-type symbol) normalized-type)
    (if (persistent-class-type-p normalized-type)
        (list +oid-sql-type+)
        (call-next-method))))

(def function compute-reader (type)
  (reader-of (compute-mapping type)))

(def generic compute-reader* (mapped-type normalized-type)
  (:method (mapped-type normalized-type)
    (error "Cannot map type ~A to a reader" mapped-type))

  (:method ((mapped-type symbol) normalized-type)
    (if (persistent-class-type-p mapped-type)
        (if *load-as-production?*
            #'object-reader
            'object-reader)
        (call-next-method))))

(def function compute-writer (type)
  (writer-of (compute-mapping type)))

(def generic compute-writer* (mapped-type normalized-type)
  (:method (mapped-type normalized-type)
    (error "Cannot map type ~A to a writer" mapped-type))

  (:method ((mapped-type symbol) normalized-type)
    (if (persistent-class-type-p mapped-type)
        (if *load-as-production?*
            #'object-writer
            'object-writer)
        (call-next-method))))

(def function compute-mapping (type)
  (unless (set-type-p type)
    (bind ((normalized-type (normalized-type-for type))
           (mapped-type (mapped-type-for normalized-type))
           (unit-types (if (eq mapped-type 'member)
                           (unit-subtypes-for type)
                           (remove-if [and (not (eq !1 'unbound))
                                           (member !1 (unit-subtypes-for mapped-type))]
                                      (unit-subtypes-for type))))
           (rdbms-types (compute-rdbms-types* mapped-type normalized-type))
           (type-tag (compute-type-tag mapped-type))
           (reader (compute-reader* mapped-type normalized-type))
           (writer (compute-writer* mapped-type normalized-type)))
      (flet ((make-mapping (&rest args)
               (apply 'make-instance 'mapping
                      :specified-type type
                      :unit-types unit-types
                      args)))
        (case (length unit-types)
          (0 (make-mapping
              :tagged #f
              :rdbms-types rdbms-types
              :nullable-types (list #f)
              :reader reader
              :writer writer))
          (1 (bind ((unit-type (first unit-types)))
               (make-mapping
                :tagged #f
                :rdbms-types rdbms-types
                :nullable-types (list #t)
                :reader (combined-reader (list (compute-reader* unit-type unit-type) reader))
                :writer (combined-writer (list (compute-writer* unit-type unit-type) writer)))))
          (t (iter (for unit-type :in unit-types)
                   (collect (compute-type-tag unit-type) :into type-tags)
                   (collect (compute-reader* unit-type unit-type) :into readers)
                   (collect (compute-writer* unit-type unit-type) :into writers)
                   (finally
                    (return
                      (make-mapping
                       :tagged #t
                       :rdbms-types (list* (sql-integer-type :bit-size 8) rdbms-types)
                       :nullable-types (list #f #t)
                       :reader (tagged-reader (append type-tags (list type-tag)) (append readers (list reader)))
                       :writer (tagged-writer (append type-tags (list type-tag)) (append writers (list writer)))))))))))))

(def function ignore-in-rdbms-equality-marker-p (value)
  (eq +ignore-in-rdbms-equality-marker+ value))

(def function lisp-value->rdbms-equality-values (type lisp-value)
  (bind ((mapping (compute-mapping type))
         (result (make-array (length (rdbms-types-of mapping)))))
    (funcall (writer-of mapping) lisp-value result 0)
    (when (tagged-p mapping)
      (dolist (unit-type (unit-types-of mapping))
        (when (typep lisp-value unit-type)
          (iter (for index :from 1 :below (length result))
                (setf (aref result index) +ignore-in-rdbms-equality-marker+)))))
    result))

(def function lisp-value->rdbms-values (type lisp-value)
  (bind ((mapping (compute-mapping type))
         (result (make-array (length (rdbms-types-of mapping)))))
    (values result (funcall (writer-of mapping) lisp-value result 0))))

(def function rdbms-values->lisp-value (type rdbms-values)
  (funcall (reader-of (compute-mapping type)) rdbms-values 0))

(def function tag-column-of (slot)
  (when (tagged-p (mapping-of slot))
    (first (columns-of slot))))

;;;;;;
;;; Type

;; TODO proper thread safety
;; TODO clear when certain stuff is cc'd
(def special-variable *mapped-types* (make-hash-table :test #'equal :synchronized #t))

(def function mapped-type-for (type)
  "Returns the smalleset supertype which is directly mapped to RDBMS based on *MAPPED-TYPE-PRECEDENCE-LIST*."
  (if (persistent-class-type-p type)
      type
      (or (gethash type *mapped-types*)
          (setf (gethash type *mapped-types*)
                (find-if [cond ((or (eq type !1)
                                    (and (listp type)
                                         (eq (first type) !1)))
                                #t)
                               ((eq 'member !1)
                                (and (listp type)
                                     (eq 'member (first type))))
                               (t
                                (subtypep type !1))]
                         *mapped-type-precedence-list*)))))

(def function normalized-type-for (type)
  "Returns a type which does not include unit types which are mapped to :null column values."
  (bind ((type (canonical-type-for type)))
    (if (or-type-p type)
        (bind ((subtypes (remove-if 'unit-type-p (cdr type))))
          (if (<= (length subtypes) 1)
              (first subtypes)
              (cons 'or subtypes)))
        (unless (unit-type-p type)
          type))))

(def function unit-type-p (type)
  (and (symbolp type)
       (bind ((mapped-type (mapped-type-for type)))
         (eq :null (first (compute-rdbms-types* mapped-type mapped-type))))))

(def function unit-subtypes-for (type)
  (collect-if (lambda (subtype)
                ;; special case for null (because of boolean and symbol)
                (if (eq subtype 'null)
                    (null-subtype-p type)
                    (and (not (member subtype '(member set disjunct-set ordered-set)))
                         (unit-type-p subtype)
                         (subtypep subtype type))))
              *mapped-type-precedence-list*))

(def function default-value-for-type (type)
  (aif (first (unit-subtypes-for type))
       (cons (bind ((expanded-type (substitute-type-arguments it nil)))
               (cond ((eq expanded-type 'null)
                      nil)
                     ((eq (first expanded-type) 'eql)
                      (second expanded-type))
                     (t
                      (error "Unknown type ~A" type))))
             #t)
       (cons nil #f)))

(def function primitive-type-p (type)
  "Accepts types such as boolean, integer, string, double, etc. which are directly mapped to RDBMS."
  (or (eq type t)
      (and (not (or-type-p type))
           (not (persistent-class-type-p type))
           (not (set-type-p* type))
           (not (unbound-subtype-p type))
           (not (null-subtype-p type)))))

(def function primitive-type-p* (type)
  "Same as primitive-type-p but also accepts values such as (or unbound integer), (or null string), (or unbound null boolean), etc."
  (primitive-type-p (normalized-type-for type)))

(def function persistent-class-type-p (type)
  "Returns true for persistent class types and false otherwise."
  (or (typep type 'persistent-class)
      (and (symbolp type)
           (find-persistent-class type))))

(def function persistent-class-type-p* (type)
  "Same as persistent-class-type-p but also accepts values such as (or unbound persistent-object), (or null persistent-object), (or unbound null persistent-object) etc."
  (persistent-class-type-p (persistent-class-type-for type)))

(def function persistent-class-type-for (type)
  (unless (eq type t)
    (canonical-type-for `(and persistent-object ,type))))

(def function or-type-p (type)
  "Returns true for type using the compound type specifier 'or'."
  (and (consp type)
       (eq 'or (first type))))

(def function set-type-p (type)
  (and (consp type)
       (eq (first type) 'set)))

(def function disjunct-set-type-p (type)
  (and (consp type)
       (eq (first type) 'disjunct-set)))

(def function ordered-set-type-p (type)
  (and (consp type)
       (eq (first type) 'ordered-set)))

(def function set-type-p* (type)
  "Returns true for all kind of persistent set types."
  (and (consp type)
       (member (first type) '(set disjunct-set ordered-set))))

(def function set-type-class-for (type)
  (second type))

(def function unbound-subtype-p (type)
  (and (not (eq 'member type))
       (subtypep 'unbound type)))

(def function null-subtype-p (type)
  ;; NOTE: null should not be treated as boolean and symbol subtype here
  ;; might return (values #f #f) but will always return (values #t #t) when required so this does not matter
  (and (not (eq 'member type))
       (or (subtypep '(satisfies null)
                     (subst '(satisfies null) 'null (canonical-type-for type)))
           (set-type-p type))))

(def function tagged-type-p (type)
  (tagged-p (compute-mapping type)))

;;;;;;
;;; Type parser

(def function parse-type (type-specifier)
  (etypecase type-specifier
    (symbol (or (find-type type-specifier :otherwise nil)
                (find-persistent-class type-specifier)))
    (list
     (apply (parser-of (class-prototype (find-class (type-class-name-for (first type-specifier)))))
            (rest type-specifier)))
    (persistent-type type-specifier)))

;;;;;;
;;; Type unparser

;; TODO: unparse it into a list
(def function unparse-type (type)
  (declare (ignore type))
  )

;;;;;;
;;; Destructure type

(def function destructure-type (type)
  "Returns (values canonical-type null-subtype-p unbound-subtype-p) corresponding to the given type."
  (bind ((normalized-type (normalized-type-for type))
         (mapped-type (mapped-type-for normalized-type))
         (unbound-subtype-p (unbound-subtype-p type))
         (null-subtype-p (and (not (null-subtype-p mapped-type))
                              (null-subtype-p type))))
    (values normalized-type null-subtype-p unbound-subtype-p)))

;;;;;;
;;; Type matcher

(def special-variable *matches-type-cut-function*)

(def function default-matches-type-cut (instance slot type)
  (declare (ignore instance slot))
  (or (persistent-object-p type)
      (set-type-p* type)))

(def function matches-type (value type &key (cut-function #'default-matches-type-cut) (signal-type-violations #f))
  (bind ((*matches-type-cut-function* cut-function))
    (flet ((body ()
             (aprog1 (matches-type* value type)
               (unless it
                 (error (make-condition 'value-type-violation :value value :value-type type))))))
      (if (not signal-type-violations)
          (handler-case (body)
            (type-violation () #f))
          (body)))))

(def condition* type-violation ()
  ())

(def condition* value-type-violation (type-violation)
  ((value
    :type t)
   (value-type
    :type the-type)))

(def condition* instance-slot-type-violation (type-violation)
  ((instance
    :type persistent-object)
   (slot
    :type persistent-effective-slot-definition)))

(def generic matches-type* (value type)
  (:documentation "Checks if the given value matches the type.")

  (:method (value type)
           (error "Value ~A could not be matched against type ~A" value type))

  (:method (value (type list))
           (typep value type)))
