;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;;;
;;; production

(def (function e) export-persistent-classes-to-database-schema ()
  (bind ((all-the-rest-confirmed? #f))
    (handler-bind ((unconfirmed-schema-change
                    (lambda (condition)
                      (when all-the-rest-confirmed?
                        (continue-with-schema-change condition)))))
      (restart-bind ((accept-all-schema-changes
                      (lambda ()
                        (setf all-the-rest-confirmed? #t)
                        (continue-with-schema-change))
                       :report-function (lambda (stream)
                                          (format stream "~@<Confirm this and all upcoming schema changes inside this call to ~S without entering the debugger~@:>" 'export-persistent-classes-to-database-schema)))
                     (abort (lambda ()
                              (return-from export-persistent-classes-to-database-schema (values)))
                       :report-function (lambda (stream)
                                          (format stream "~@<Cancel ~S and return with (values)~@:>" 'export-persistent-classes-to-database-schema))))
        (maphash-values 'ensure-exported *persistent-classes*)
        (finalize-persistent-classes)
        (finalize-persistent-associations)))))

;;;;;;
;;; API

(def (generic e) export-persistent-instances (thing format stream &key &allow-other-keys))

(def (generic e) import-persistent-instances (format stream &key &allow-other-keys))

;;;;;;
;;; Serializer

(def constant +persistent-object-code+ #x61)

(def method export-persistent-instances (thing (format (eql :binary)) stream &key (persistent-object-serializer #'write-persistent-object-slot-values) &allow-other-keys)
  (hu.dwim.serializer:serialize thing :output stream :serializer-mapper (make-export-serializer-mapper persistent-object-serializer)))

(def method import-persistent-instances ((format (eql :binary)) stream &key (persistent-object-deserializer #'read-persistent-object-slot-values) &allow-other-keys)
  (hu.dwim.serializer:deserialize stream :deserializer-mapper (make-export-deserializer-mapper persistent-object-deserializer)))

(def (function o) make-export-serializer-mapper (persistent-object-serializer)
  (lambda (instance context)
    (bind (((:values code has-identity writer-function)
            (hu.dwim.serializer::default-serializer-mapper instance context)))
      (if (and (eq code hu.dwim.serializer::+standard-object-code+)
               (typep instance 'persistent-object))
          (values +persistent-object-code+ #t
                  (lambda (instance context)
                    (bind ((class (class-of instance)))
                      (hu.dwim.serializer::serialize-symbol (class-name class) context)
                      (funcall persistent-object-serializer instance context))))
          (values code has-identity writer-function)))))

(def (function o) make-export-deserializer-mapper (persistent-object-deserializer)
  (lambda (code context)
    (if (eq code +persistent-object-code+)
        (lambda (context &optional referenced)
          (declare (ignore referenced))
          (bind ((class-name (hu.dwim.serializer::deserialize-symbol context))
                 (class (find-class class-name :errorp #f))
                 (prototype-or-class-name (or (and class (closer-mop:class-prototype class))
                                              class-name)))
            (funcall persistent-object-deserializer prototype-or-class-name context)))
        (hu.dwim.serializer::default-deserializer-mapper code context))))

(def (function e) write-persistent-object-slot-values (instance context &key exclude-slots)
  (bind ((class (class-of instance))
         (slots (collect-if (lambda (slot)
                              (and (persistent-slot-p slot)
                                   (not (eq (closer-mop:slot-definition-allocation slot) :class))
                                   (not (member (closer-mop:slot-definition-name slot) exclude-slots))))
                            (class-slots class))))
    (write-persistent-object-oid (oid-of instance) context)
    (hu.dwim.serializer::write-variable-length-positive-integer (length slots) context)
    (dolist (slot slots)
      (hu.dwim.serializer::serialize-symbol (closer-mop:slot-definition-name slot) context)
      (if (closer-mop:slot-boundp-using-class class instance slot)
          (hu.dwim.serializer::serialize-element (closer-mop:slot-value-using-class class instance slot) context)
          (hu.dwim.serializer::write-unsigned-byte-8 hu.dwim.serializer::+unbound-slot-code+ context)))))

(def (function e) read-persistent-object-slot-values (prototype-or-class-name context &optional (persistp #t))
  (bind ((class (etypecase prototype-or-class-name
                  (symbol (find-class prototype-or-class-name))
                  (persistent-object (class-of prototype-or-class-name))))
         (oid (make-new-oid class))
         (instance (allocate-instance class))
         (old-oid (read-persistent-object-oid context)))
    (initialize-revived-instance instance :persistent #f :oid oid)
    (hu.dwim.serializer::announce-identity instance context)
    (iter (repeat (the fixnum (hu.dwim.serializer::read-variable-length-positive-integer context)))
          (for slot-name = (hu.dwim.serializer::deserialize-symbol context))
          (if (eq hu.dwim.serializer::+unbound-slot-code+ (hu.dwim.serializer::read-unsigned-byte-8 context))
              (slot-makunbound instance slot-name)
              (setf (slot-value instance slot-name)
                    (progn
                      (hu.dwim.serializer::unread-unsigned-byte-8 context)
                      (hu.dwim.serializer::deserialize-element context)))))
    (when persistp
      (make-persistent instance))
    (values instance old-oid)))

(def (function e) dump-persistent-object-slot-values (prototype-or-class-name context)
  (bind ((class (etypecase prototype-or-class-name
                  (symbol (find-class prototype-or-class-name))
                  (persistent-object (class-of prototype-or-class-name))))
         (class-name (class-name class))
         (oid (read-persistent-object-oid context))
         (instance (list :object class-name oid)))
    (hu.dwim.serializer::announce-identity (list :reference class-name oid) context)
    (iter (repeat (the fixnum (hu.dwim.serializer::read-variable-length-positive-integer context)))
          (for slot-name = (hu.dwim.serializer::deserialize-symbol context))
          (nconcf instance
                  (list (intern (symbol-name slot-name) :keyword)
                        (if (eq hu.dwim.serializer::+unbound-slot-code+ (hu.dwim.serializer::read-unsigned-byte-8 context))
                            :unbound
                            (progn
                              (hu.dwim.serializer::unread-unsigned-byte-8 context)
                              (hu.dwim.serializer::deserialize-element context))))))
    instance))
