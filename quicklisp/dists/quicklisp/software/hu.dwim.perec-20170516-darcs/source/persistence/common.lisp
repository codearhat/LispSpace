;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

(def special-variable *exporting-to-rdbms* #f)

(def computed-class* exportable ()
  ((ensure-exported
    (compute-as (export-to-rdbms -self-) -self-)
    :reader ensure-exported
    :documentation "A persistent class, a persistent association and the related tables must be exported before use. This will automatically happen not later than making, reviving, querying or using by any means the first instance of it.")))

(def generic export-to-rdbms (instance)
  (:documentation "Exports classes, associations, tables to the database, may create new tables or alter existing ones.")

  (:method :around (instance)
           (if *exporting-to-rdbms*
               (call-next-method)
               (let ((*exporting-to-rdbms* #t))
                 (with-transaction
                   (call-next-method))))))

(def function ensure-all-computed-slots-are-valid (thing)
  (bind ((class (class-of thing)))
    (dolist (slot (class-slots class))
      (when (typep slot 'computed-effective-slot-definition)
        (slot-value-using-class class thing slot)))))
