;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms)

;;;;;;
;;; Database

(def (class* e) database ()
  ((connection-specification
    :documentation "Backend specific connection data, usually a plist of args passed to the connect function."
    :export :accessor)
   (default-result-type
    'vector
    :type (member vector list cursor))
   (transaction-class
    :type standard-class
    :documentation "Transactions will be instances of this class. This class is created according to the generic method transaction-mixin-class.")
   (encoding
    :utf-8
    :type (member :utf-8 :us-ascii))
   ;; TODO think through this ddl-query-cache thing...
   (ddl-query-cache
    nil
    :documentation "A cache to speed up querying the metadata of the database (its schema)."
    :type (or hash-table null))))

(def (constant e) +database-command-line-options+
  '((("database-host" #\Space)
     :type string
     :initial-value "localhost"
     :documentation "The server host name where the database is listening.")
    (("database-port" #\Space)
     :type integer
     :documentation "The server port where the database is listening.")
    (("database-name" #\Space)
     :type string
     :documentation "The database name that will be connected.")
    (("database-user-name" #\Space)
     :type string
     :documentation "The user name that is used to connect to the database.")
    (("database-password" #\Space)
     :type string
     :documentation "The password that is used to connect to the database.")))

(def method shared-initialize :after ((database database) slot-names
                                     &key transaction-mixin generated-transaction-class-name &allow-other-keys)
  (let ((classes (mapcar #'find-class (transaction-mixin-class database))))
    (setf (transaction-class-of database)
          (make-instance 'standard-class
                         :name generated-transaction-class-name
                         :direct-superclasses (aif transaction-mixin
                                                   (cons (find-class it) classes)
                                                   classes)))))

(def (generic e) transaction-mixin-class (database)
  (:documentation "Collects the transaction mixin classes which will be inherited by the transaction class instantiated by with-transaction when using this database.")

  (:method-combination list))

(def (macro e) with-database (database &body forms)
  "Evaluates FORMS within the dynamic scope of DATABASE."
  `(let ((*database* ,database))
     (assert (typep *database* 'database) () "~S was called with something that is not typep 'database: ~A" 'with-database *database*)
     ,@forms))

;;;;;;
;;; RDBMS names

(def generic calculate-rdbms-name (database thing name)
  (:documentation "May be specialized to take name length and character set limitations into account.")
  (:method ((database database) thing name)
           (string-downcase name)))

(def function rdbms-name-for (name &optional thing)
  (declare (cl:type (or null (member :table :view :index :column :sequence)) thing))
  (calculate-rdbms-name *database* thing name))

(def function calculate-rdbms-name-with-utf-8-length-limit (name limit &key prefix)
  "Cuts off the end of names that are too long and appends the hash of the original name."
  (assert (>= limit 8))
  (bind ((name-as-string (string+ prefix (string-downcase name))))
    (iter (for char :in-sequence "+*\\/-~%")
          (nsubstitute #\_ char name-as-string :test #'char=))
    (bind ((name-as-bytes (string-to-octets name-as-string :encoding :utf-8)))
      (when (> (length name-as-bytes)
               limit)
        (bind ((hash (ironclad:byte-array-to-hex-string
                      (ironclad:digest-sequence :crc32 name-as-bytes))))
          ;; drop chars one-by-one because we have no idea about their UTF-8 length
          (iter (while (> (length name-as-bytes)
                          (- limit 8)))
                (setf name-as-string (subseq name-as-string 0 (1- (length name-as-string))))
                (setf name-as-bytes (string-to-octets name-as-string :encoding :utf-8)))
          (setf name-as-string (string+ name-as-string hash))))
      name-as-string)))

#+nil ; TODO this version is much better, but it's not backwards compatible, so keep it commented out for now...
(def function calculate-rdbms-name-with-utf-8-length-limit (name length-limit &key prefix (illegal-characters "+*\\/-~%")
                                                                 (hash-separator "_"))
  "When needed, drops characters from the middle of names and replaces them with a hash code of the uncut name."
  (check-type name string)
  (assert (every [or (lower-case-p !1) (digit-char-p !1) (not (alphanumericp !1))] name) () "RDBMS names must always be lower case, because some backends ignore the case: ~S" name)
  (flet ((compress (vector ratio)
           "Compress byte vectors by xor'ing the dropped elements to the remaining elements."
           (check-type ratio positive-integer)
           (check-type vector (vector (unsigned-byte 8) *))
           (bind ((new-length (/ (length vector) ratio))
                  (new-vector (make-array new-length :element-type '(unsigned-byte 8))))
             (check-type new-length positive-integer)
             (loop
               :for index :from 0 :below new-length
               :do (loop
                     :for factor :from 1 :below ratio
                     :for dropped-index = (+ index (* factor new-length))
                     :do (setf (aref new-vector index) (logxor (aref vector index)
                                                               (aref vector dropped-index)))))
             new-vector)))
    (bind ((name-as-string (string+ prefix name)))
      (iter (for char :in-sequence illegal-characters)
            (nsubstitute #\_ char name-as-string :test #'char=))
      (bind ((name-as-bytes (string-to-octets name-as-string :encoding :utf-8)))
        (when (> (length name-as-bytes)
                 length-limit)
          ;; drop the resolution of upper/lower case when calculating the hash
          (bind ((hash (compress (ironclad:digest-sequence :sha256 (string-to-octets name :encoding :utf-8)) 8))
                 (hash-as-string (ironclad:byte-array-to-hex-string hash))
                 (hash-length (length hash-as-string))
                 (chopped-name name-as-string)
                 (extra-character-count (* 2 (length hash-separator))))
            (assert (<= (+ hash-length extra-character-count) length-limit))
            ;; drop chars one-by-one because we have no idea about their UTF-8 length
            (iter (while (> (length name-as-bytes)
                            (- length-limit hash-length)))
                  (for middle-index = (floor (/ (length chopped-name) 2)))
                  (setf chopped-name (string+ (subseq chopped-name 0 middle-index)
                                              (subseq chopped-name (1+ middle-index))))
                  (setf name-as-bytes (string-to-octets chopped-name :encoding :utf-8)))
            (bind ((part-size (floor (/ (- (length chopped-name) extra-character-count) 2))))
              (setf name-as-string (string+ (subseq chopped-name 0 part-size)
                                            hash-separator
                                            hash-as-string
                                            hash-separator
                                            (subseq chopped-name (+ extra-character-count part-size))))))
          (assert (= (length name-as-string) length-limit))))
      (assert (<= (length name-as-string) length-limit))
      name-as-string)))

(def (function e) enable-ddl-query-cache (database)
  (setf (ddl-query-cache-of database) (make-hash-table)))

(def (function e) disable-ddl-query-cache (database)
  (setf (ddl-query-cache-of database) nil))

(def (function e) clear-ddl-query-cache (database)
  (disable-ddl-query-cache database)
  (enable-ddl-query-cache database))
