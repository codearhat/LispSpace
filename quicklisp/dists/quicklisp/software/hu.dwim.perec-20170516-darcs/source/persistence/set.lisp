;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;;;
;;; Set

;; TODO: enforce disjunct-set in defpassociation for 1-N associations mapped as foreign keys in the RDBMS?

;; TODO: set types need some refactoring, to have a common base type and to be less fragile
(def persistent-type set (&optional element-type)
  (declare (ignore element-type))
  '(or list persistent-slot-set-container))

(def method shared-initialize :around ((type set-type) slot-names &rest args &key element-type &allow-other-keys)
  (apply #'call-next-method type slot-names :element-type (parse-type element-type) (remove-from-plist args :element-type)))

;; TODO: distinguish between set type and disjunct set type (the latter used in 1-n associations for example)
;; TODO: assert 1-n associations use disjunct-set type
(def persistent-type disjunct-set (&optional element-type)
  (declare (ignore element-type))
  '(or list persistent-slot-set-container))

(def method shared-initialize :around ((type disjunct-set-type) slot-names &rest args &key element-type &allow-other-keys)
  (apply #'call-next-method type slot-names :element-type (parse-type element-type) (remove-from-plist args :element-type)))

(def function ordered-set-p (instance)
  (declare (ignore instance))
  #t)

(def persistent-type ordered-set (&optional element-type by)
  (declare (ignore element-type by))
  '(and (satisfies ordered-set-p)
        (or list persistent-slot-set-container)))

(def method shared-initialize :around ((type ordered-set-type) slot-names &rest args &key element-type &allow-other-keys)
  (apply #'call-next-method type slot-names :element-type (parse-type element-type) (remove-from-plist args :element-type)))

;;;;;;
;;; Lazy slot set container

(def class* persistent-slot-set-container (set-container)
  ((instance
    :type persistent-object)
   (slot
    :type persistent-effective-slot-definition)))

(def generic check-insert-item (set item)
  (:method ((set persistent-slot-set-container) item)
    (when (find-item set item)
      (error "The item ~A is already in the association end set ~A" item set))))

(def method insert-item :before ((set persistent-slot-set-container) item)
  (check-insert-item set item))

(def method insert-item ((set persistent-slot-set-container) (item persistent-object))
  (bind ((slot (slot-of set))
         (instance (instance-of set)))
    ;; TODO: this is incorrect, add test?
    (check-slot-value-type instance slot item)
    (let ((rdbms-values (make-array +oid-column-count+)))
      (object-writer instance rdbms-values 0)
      (dolist (table (association-end-tables-of slot))
        (update-records (name-of table)
                        (columns-of slot)
                        rdbms-values
                        (make-oid-matcher-where-clause item))))))

(def method ensure-item ((set persistent-slot-set-container) (item persistent-object))
  (unless (find-item set item)
    (insert-item set item)))

(def generic check-delete-item (set item)
  (:method ((set persistent-slot-set-container) item)
    (unless (find-item set item)
      (error "The item ~A is not in the association end set ~A" item set))))

(def method delete-item :before ((set persistent-slot-set-container) item)
  (check-delete-item set item))

(def method delete-item ((set persistent-slot-set-container) (item persistent-object))
  (bind ((slot (slot-of set)))
    (check-slot-value-type (instance-of set) slot item)
    (dolist (table (association-end-tables-of slot))
      (update-records (name-of table)
                      (columns-of slot)
                      '(nil nil)
                      (make-oid-matcher-where-clause item)))))

(def method find-item ((set persistent-slot-set-container) (item persistent-object))
  (bind ((slot (slot-of set)))
    (not (zerop (select-count-* (list (name-of (association-end-view-of (other-association-end-of slot))))
                                (sql-and (make-oid-matcher-where-clause (instance-of set) (oid-column-of slot))
                                         (make-oid-matcher-where-clause item)))))))

(def method size ((set persistent-slot-set-container))
  (bind ((slot (slot-of set)))
    (select-count-* (list (name-of (association-end-view-of (other-association-end-of slot))))
                    (make-oid-matcher-where-clause (instance-of set) (oid-column-of slot)))))

(def method empty-p ((set persistent-slot-set-container))
  (= 0 (size set)))

(def method empty! ((set persistent-slot-set-container))
  (delete-slot-set (instance-of set) (slot-of set)))

(def method list-of ((set persistent-slot-set-container))
  (restore-slot-set (instance-of set) (slot-of set)))

(def method (setf list-of) (new-value (set persistent-slot-set-container))
  (store-slot-set (instance-of set) (slot-of set) new-value))

(def method iterate-items ((set persistent-slot-set-container) function)
  (mapc function (list-of set)))

;;;;;;
;;; Lazy persistent set with identity

(def persistent-class persistent-set-element ()
  ()
  (:abstract #t)
  (:direct-store :push-down))

(def persistent-class persistent-set ()
  ())

(def persistent-association*
  ((:class persistent-set :slot items :type (set persistent-set-element))
   (:class persistent-set-element :slot sets :type (set persistent-set))))

(def method insert-item ((set persistent-set) (item persistent-set-element))
  (insert-item (items-of* set) item))

(def method delete-item ((set persistent-set) (item persistent-set-element))
  (delete-item (items-of* set) item))

(def method find-item ((set persistent-set) (item persistent-set-element))
  (find-item (items-of* set) item))

(def method size ((set persistent-set))
  (with-lazy-slot-value-collections
    (size (items-of set))))

(def method empty-p ((set persistent-set))
  (= 0 (size set)))

(def method empty! ((set persistent-set))
  (with-lazy-slot-value-collections
    (empty! (items-of set))))

(def method list-of ((set persistent-set))
  (items-of set))

(def method (setf list-of) (items (set persistent-set))
  (setf (items-of set) items))

(def method iterate-items ((set persistent-set) function)
  (mapc function (items-of set)))
