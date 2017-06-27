;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;;;
;;; XML

(enable-sharp-boolean-syntax)

(hu.dwim.quasi-quote.xml:enable-quasi-quoted-xml-syntax
 :transformation-pipeline (hu.dwim.quasi-quote.xml:make-quasi-quoted-xml-to-form-emitting-transformation-pipeline
                           '*xml-stream*
                           :binary t
                           :encoding :utf-8
                           :with-inline-emitting t
                           :indentation-width 2))

(def special-variable *xml-stream*)

(def method export-persistent-instances (thing (format (eql :xml)) stream &key &allow-other-keys)
  (bind ((*xml-stream* stream)
         (seen-set (make-hash-table)))
    (labels ((recurse (thing)
               (if (gethash thing seen-set)
                   <reference (:oid ,(oid-of thing))>
                   (etypecase thing
                     (list <list ,(mapc #'recurse thing)>)
                     (persistent-object
                      (bind ((instance thing)
                             (class (class-of instance))
                             (class-name (string-downcase (class-name class)))
                             (slots (remove-if (of-type 'persistent-effective-slot-definition-d) (persistent-effective-slots-of class))))
                        (setf (gethash thing seen-set) #t)
                        <,class-name (:oid ,(oid-of instance)
                                           ,@(iter (for slot :in slots)
                                                   (when (and (not (typep slot 'persistent-association-end-effective-slot-definition))
                                                              (not (persistent-class-type-p* (canonical-type-of slot)))
                                                              (closer-mop:slot-boundp-using-class class instance slot))
                                                     (bind ((slot-name (string-downcase (symbol-name (closer-mop:slot-definition-name slot))))
                                                            (value (closer-mop:slot-value-using-class class instance slot))
                                                            (slot-value (etypecase value
                                                                          ((member #f #t) (if value "#t" "#f"))
                                                                          (string value)
                                                                          (symbol (symbol->canonical-name value))
                                                                          (number (princ-to-string value))
                                                                          (timestamp (format-timestring nil value
                                                                                                        :format '((:year 4) #\- (:month 2) #\- (:day 2) #\Space
                                                                                                                  (:hour 2) #\: (:min 2) #\: (:sec 2) #\.
                                                                                                                  (:usec 6) :gmt-offset)
                                                                                                        :timezone +utc-zone+)))))
                                                       (collect (hu.dwim.quasi-quote.xml:make-xml-attribute slot-name slot-value))))))
                                     ,(iter (for slot :in slots)
                                            (when (and (typep slot 'persistent-association-end-effective-slot-definition)
                                                       (closer-mop:slot-boundp-using-class class instance slot))
                                              (bind ((slot-name (string-downcase (symbol-name (closer-mop:slot-definition-name slot))))
                                                     (value (closer-mop:slot-value-using-class class instance slot)))
                                                (if value
                                                    <,slot-name ,(ecase (cardinality-kind-of slot)
                                                                        (:1 (recurse value))
                                                                        (:n (map nil #'recurse value)))>
                                                    <,slot-name>))))>))))))
      (recurse thing))))

(def method import-persistent-instances ((format (eql :xml)) stream &key &allow-other-keys)
  (not-yet-implemented))
