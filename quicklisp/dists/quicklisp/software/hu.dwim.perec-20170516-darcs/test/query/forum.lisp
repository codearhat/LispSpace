;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

;;;; TODO: specify expected result in tests
(in-suite test/query/select)

(def persistent-class* topic-test ()
  ((title :type (text 50))))

(def persistent-class* message-test ()
  ((subject :type (text 50))
   (content :type (text 50))))

(def persistent-class* ad-test (message-test)
  ())

(def persistent-class* spam-test (ad-test)
  ((score :type integer-32)
   (spam-type :type (member phishing money-from-africa viagra))))

(def persistent-class* owner-test ()
  ((name :type (text 50)))
  (:abstract #t))

(def persistent-class* user-test (owner-test)
  ((password :type (text 50))
   (birthday :type date)
   (age 32 :persistent #f :type integer-32)))

(def persistent-association*
  ((:class topic-test :slot messages :type (set message-test))
   (:class message-test :slot topic :type topic-test)))

(def persistent-association*
  ((:class owner-test :slot topics :type (set topic-test))
   (:class topic-test :slot owner :type owner-test)))

;; PORT:
(def fixture forum-data
  (with-transaction
    (purge-instances 'persistent-object)
    (bind ((user1 (make-instance 'user-test
                                 :name "user1"
                                 :birthday (parse-timestring "1984-04-22T00:00:00Z")
                                 :password "secret"))
           (topic1 (make-instance 'topic-test :title "topic1" :owner user1)))
      (make-instance 'user-test
                     :name "user2"
                     :birthday (parse-timestring "1975-07-02T00:00:00Z")
                     :password "sglF$%3D")
      (make-instance 'topic-test :title "topic2" :owner user1)
      (make-instance 'message-test :subject "subject1" :content "content1" :topic topic1)
      (make-instance 'message-test :subject "subject2" :content "content2" :topic topic1)
      (make-instance 'ad-test :subject "ad1" :content "content3" :topic topic1)
      (make-instance 'spam-test :subject "spam1" :content "content4" :topic topic1 :score 10 :spam-type 'viagra)))
  (-body-))

(def test test/query/select/simple ()
  (test-query (:record-count 4 :fixture forum-data)
    (select (o)
      (from o)
      (where (typep o 'message-test)))))

(def test test/query/select/short ()
  (test-query (:record-count 4 :fixture forum-data)
    (select (o)
      (from (o message-test)))))

(def test test/query/select/all ()
  (test-query (:fixture forum-data)
    (select (o)
      (from o))))

(def test test/query/select/volatile ()
  (run-queries
    (with-fixture forum-data
      (bind ((title "topic2"))
        (is (equal (title-of (the-only-element (finishes (select (o)
                                                           (from (o topic-test))
                                                           (where (equal (title-of o) (volatile title)))))))
                   title))))))

(def test test/query/select/symbol-enum ()
  (test-query (:select-count 1 :record-count 1 :fixture forum-data)
    (select (s)
      (from (s spam-test))
      (where (eq (spam-type-of s) 'viagra)))))

(def test test/query/select/slot-value ()
  (test-query (:select-count 1 :record-count 1 :fixture forum-data)
    (select (m)
      (from (m message-test))
      (where (string= (subject-of m) "subject1")))))

(def test test/query/select/nested-ands ()
  (test-query (:record-count 4 :fixture forum-data)
    (select (object)
      (from object)
      (where (and t (and t (typep object 'message-test)))))))

(def test test/query/select/<= ()
  (test-query (:select-count 1 :record-count 0 :fixture forum-data)
    (select (s)
      (from (s spam-test))
      (where (<= 50 (score-of s) 100))))

  (test-query (:select-count 1 :record-count 1)
    (select (s)
      (from (s spam-test))
      (where (not (<= 50 (score-of s) 100))))))

(def test test/query/select/multiple-objects ()
  (test-query (:record-count (* 2 4 2) :fixture forum-data)
    (select (user message topic)
      (from (user user-test) (message message-test) (topic topic-test)))))

(def test test/query/select/with-lexical-variables-1 ()
  (test-query (:record-count 1 :fixture forum-data)
    (let ((user-name "user1"))
      (select (user)
        (from (user user-test))
        (where (equal (name-of user) user-name))))))

(def test test/query/select/with-lexical-variables-2 ()
  (test-query (:record-count 4 :fixture forum-data)
    (let ((class (find-class 'message-test)))
      (select (o)
        (from o)
        (where (typep o (class-name class)))))))

(def test test/query/select/with-dynamic-variables ()
  (with-fixture forum-data
    (let ((user (with-transaction (select-first-matching-instance user-test))))
      (test-query (:select-count nil :record-count 2)
        (revive-instance user)          ; for eq
        (select (o)
          (from (o topic-test))
          (where (eq (owner-of o) user)))))))

(def test test/query/select/with-literal-object ()
  (with-fixture forum-data
    (bind ((user (with-transaction (select-first-matching-instance user-test))))
      (test-query (:select-count nil :record-count 2)
        (execute-query
         (make-query
          `(select (o)
             (from (o topic-test))
             (where (eq (owner-of o) (first-arg ,user o))))))))))

(def test test/query/select/polimorph-association-end ()
  (test-query (:select-count (+ 2 1) :record-count 2 :fixture forum-data)
    (let ((topic (select-first-matching-instance topic-test)))
      (select (o)
        (from (o ad-test))
        (where (eq (topic-of o) topic))))))

(def test test/query/select/slot ()
  (test-query (:record-count 2 :fixture forum-data)
    (select ((name-of user))
      (from (user user-test)))))

(def test test/query/select/simple-association ()
  (test-query (:select-count nil :record-count 2 :fixture forum-data)
    (select ((owner-of topic))
      (from (topic topic-test)))))

(def test test/query/select/or ()
  (test-query (:record-count (+ 4 2) :fixture forum-data)
    (select (o)
      (from o)
      (where (or (typep o 'message-test) (typep o 'topic-test))))))

(def test test/query/select/builder ()
  (test-query (:record-count 4 :fixture forum-data)
    (let ((query (make-query nil)))
      (add-query-variable query 'm)
      (add-assert query '(typep m 'message-test))
      (add-collect query 'm)
      (execute-query query))))

(def test test/query/select/join-1 ()
  (test-query (:record-count 4 :fixture forum-data)
    (select (message)
      (from (message message-test))
      (where (equal (title-of (topic-of message)) "topic1")))))

(def test test/query/select/join-n/collect-child ()
  (test-query (:record-count 4 :fixture forum-data)
    (select (message)
      (from (topic topic-test) (message message-test))
      (where (and
              (equal (title-of topic) "topic1")
              (member message (messages-of topic)))))))

(def test test/query/select/join-n/collect-parent ()
  (test-query (:record-count 1 :fixture forum-data)
    (select (topic)
      (from (topic topic-test) (message message-test))
      (where (and
              (equal (title-of topic) "topic1")
              (member message (messages-of topic))
              (equal (subject-of message) "subject1"))))))

(def test test/query/select/join-n-in-collect ()
  (test-query (:select-count nil :record-count 1 :fixture forum-data)
    (select ((messages-of topic))
      (from (topic topic-test))
      (where (equal (title-of topic) "topic1")))))

#|
(def test test/query/select/general ()
  (test-query (:select-count nil :record-count 1 :fixture forum-data)
    (select ((topic topic-test))
      (when (equal (title-of topic) "topic1")
        (collect topic)))))
|#

(def test test/query/select/with-lisp-filter ()
  (test-query (:select-count nil :record-count 1 :fixture forum-data)
    (let ((predicate (lambda (message) (equal (subject-of message) "subject1"))))
      (select (message)
        (from (message message-test))
        (where (funcall predicate message))))))

(def test test/query/select/cnf ()
  (test-query (:record-count 1 :fixture forum-data)
    (select (m)
      (from (m message-test))
      (where (or (not (equal (title-of (topic-of m)) "topic1"))
                 (and (equal (subject-of m) "subject1")
                      (equal (content-of m) "content1")))))))

(def test test/query/select/typep-1 ()
  (test-query (:record-count 2 :fixture forum-data)
    (let ((subject "subject1"))
      (select (m)
        (from (m message-test))
        (where (and (or (typep m 'ad-test)
                         (equal (subject-of m) subject))
                     (not (typep m 'spam-test))))))))

(def test test/query/select/typep-2 ()
  (test-query (:select-count nil :record-count 1 :fixture forum-data)
    (bind ((instance (select-first-matching-instance user-test)))
      (select (o)
        (from o)
        (where (and (typep o (class-name (class-of instance)))
                    ;; commenting out the next clause makes it work.
                    ;; also note that it does not do what one would expect...
                    #+nil(not (eq o instance))
                    ;; while this one does what is expected (not (equal (oid-of o) (oid-of instance)))
                    (not (equal (oid-of o) (oid-of instance)))
                    ))))))

(def test test/query/select/macro ()
  (define-query-macro s-of (message)
    `(subject-of ,message))
  (test-query (:record-count 1 :fixture forum-data)
    (select (m)
      (from (m message-test))
      (where (equal (s-of m) "subject1")))))

(def test test/query/select/count ()
  (test-query (:record-count 1 :fixture forum-data)
    (select (topic)
      (from (topic topic-test))
      (where (>= (length (messages-of topic)) 2)))))

(def test test/query/select/member-1 ()
  (test-query (:select-count 2 :record-count 3 :fixture forum-data)
    (let ((messages (cdr (select-instances message-test))))
      (select (m)
        (from (m message-test))
        (where (member m messages))))))

(def test test/query/select/member-2 ()
  (test-query (:record-count 1 :fixture forum-data)
    (select (m)
      (from (m message-test))
      (where (member (subject-of m) '("subject1" "no-such-subject") :test 'equal)))))

(def test test/query/select/member-3 ()
  (test-query (:select-count 0 :record-count 0 :fixture forum-data)
    (bind ((*enable-partial-eval* #t))
      (select (m)
       (from (m message-test))
       (where (member (subject-of m) nil))))))

(def test test/query/select/member-4 ()
  (test-query (:select-count 1 :record-count 0 :fixture forum-data)
    (bind ((*enable-partial-eval* #t))
      (let ((topics (select-instances topic-test)))
       (execute-query
        (make-query
         `(select (m)
            (from (m message-test))
            (where (member m ',topics)))))))))

(def test test/query/select/member-5 ()
  (with-expected-failures
    (test-query (:select-count 3 :record-count 1 :fixture forum-data)
      (let ((list (append (select-instances topic-test) (select-instances spam-test))))
        (execute-query
         (make-query
          `(select (m)
             (from (m message-test))
             ;; FIXME? attila: i *think* this fails because when its executed in lisp then MEMBER uses #'EQL,
             ;; but the instances are only EQL (EQ in fact) when everything runs in the same transaction.
             ;; also see the note in the defmethod for %COMPILE-QUERY specialized on DEBUG-QUERY-COMPILER
             (where (member m ',list)))))))))

(def test test/query/select/member-6 ()
  (signals error
    (run-queries
      (select (s)
        (from (s spam-test))
        (where (eq (spam-type-of s) 'no-such-member))))))

(def test test/query/select/lisp-expr ()
  (test-query (:record-count 1 :fixture forum-data)
    (let ((num "1"))
      (select (topic)
        (from (topic topic-test))
        (where (equal (title-of topic) (string+ "topic" num)))))))

(def test test/query/select/like ()
  (test-query (:select-count 1 :record-count 1 :fixture forum-data)
    (select (topic)
      (from (topic topic-test))
      (where (like (title-of topic) "t%pi_1")))))

(def test test/query/select/string+ ()
  (test-query (:select-count 1 :record-count 1 :fixture forum-data)
    (select (tc m u)
      (from (tc topic-test) (m message-test) (u user-test))
      (where (equal (string+ (title-of tc) ":" (subject-of m) ":" (name-of u))
                    "topic1:subject1:user1")))))

(def test test/query/select/non-persistent-slot ()
  (test-query (:record-count 2 :fixture forum-data) ; TODO: should select 1
    (select (u)
      (from (u user-test))
      (where (> (age-of u) 30)))))

(def test test/query/select/max ()
  (test-query (:record-count 1 :fixture forum-data)
    (select ((max (score-of s)))
      (from (s spam-test)))))

(def test test/query/select/clash-with-lexical-variable ()
  (test-query (:record-count 4 :fixture forum-data)
    (let ((o "this should not make any headaches"))
      (select (o)
        (from o)
        (where (typep o 'message-test))))))

#+nil
(def test test/query/select/subselect ()
  (test-query (:select-count 1 :record-count 4 :fixture forum-data)
    (select (m)
      (from (m message-test))
      (where (eq (topic-of m) (select (topic)
                                (from (topic topic-test))
                                (where (equal (title-of topic) "topic1"))))))))


(def test test/query/select/equal/instance ()
  (with-expected-failures
    (test-query (:select-count nil :record-count 1 :fixture forum-data)
      (bind ((instance (with-transaction (select-instance message-test (where (equal (subject-of -instance-) "subject1")))))
             (slot-name 'subject)
             (pattern "subject_"))
        (select (o)
          (from o)
          (where (and (typep o (class-name (class-of instance)))
                      (not (equal o instance)) ;; FIXME this does not work, but (not (equal (oid-of o) (oid-of instance))) works as expected.
                      ;; reason: when executed in lisp O and INSTANCE are not equal (only P-EQ)
                      (like (slot-value o slot-name) pattern))))))))

(deftest test/query/select/select-instance/bug ()
  (test-query (:select-count nil :fixture forum-data)
    (bind ((oids (select ((oid-of instance)) (from (instance message-test)))))
      (select-instances (o)
        (where (member (oid-of o) oids))))))
