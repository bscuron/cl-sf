;;;; cl-sf/tests/objects-tests.lisp
;;;; Unit tests for cl-sf/objects — CLOS layer, conversions, class registry.

(in-package #:cl-sf/tests)

;;; ── Test fixture class ────────────────────────────────────────────

(cl-sf/objects:define-sobject-class test-contact "TestContact"
  (first-name   "FirstName"    :type (or string null))
  (last-name    "LastName"     :type (or string null))
  (email        "Email"        :type (or string null))
  (annual-salary "AnnualSalary" :type (or number null)))

(define-test objects
  :parent cl-sf/tests
  :description "CLOS layer — class definition, hash→sobject, sobject→hash, field names.")

;;; ──────────────────────────────────────────────────────────────────
;;; define-sobject-class
;;; ──────────────────────────────────────────────────────────────────

(define-test define-sobject-class/creates-class
  :parent objects
  (true (find-class 'test-contact nil)
        "test-contact class exists after define-sobject-class"))

(define-test define-sobject-class/is-sobject-subclass
  :parent objects
  (true (subtypep 'test-contact 'cl-sf/objects:sobject)
        "test-contact is a subclass of sobject"))

(define-test define-sobject-class/registered-in-classes
  :parent objects
  (let ((class (gethash "TestContact" cl-sf/objects:*sobject-classes*)))
    (true class "class registered under Salesforce API name")
    (is eq (find-class 'test-contact) class)))

(define-test define-sobject-class/slot-map-registered
  :parent objects
  (let ((slot-map (gethash 'test-contact cl-sf/objects::*sobject-slot-maps*)))
    (true slot-map "slot map registered for test-contact")
    (true (assoc "FirstName"    slot-map :test #'equal))
    (true (assoc "LastName"     slot-map :test #'equal))
    (true (assoc "Email"        slot-map :test #'equal))
    (true (assoc "AnnualSalary" slot-map :test #'equal))))

(define-test define-sobject-class/accessor-works
  :parent objects
  (let ((obj (make-instance 'test-contact :first-name "Ada")))
    (is string= "Ada" (test-contact-first-name obj)
        "Generated accessor returns slot value")))

(define-test define-sobject-class/default-type-set
  :parent objects
  (let ((obj (make-instance 'test-contact)))
    (is string= "TestContact" (cl-sf/objects:sobject-type obj)
        ":default-initargs sets type to SF API name")))

;;; ──────────────────────────────────────────────────────────────────
;;; hash->sobject
;;; ──────────────────────────────────────────────────────────────────

(define-test hash->sobject/registered-type
  :parent objects
  (let* ((hash (make-hash "Id" "003xx" "FirstName" "Grace" "LastName" "Hopper"))
         (obj  (cl-sf/objects:hash->sobject hash "TestContact")))
    (of-type test-contact obj "Returns typed test-contact instance")
    (is string= "003xx"  (cl-sf/objects:sobject-id obj))
    (is string= "Grace"  (test-contact-first-name obj))
    (is string= "Hopper" (test-contact-last-name  obj))))

(define-test hash->sobject/type-from-attributes
  :parent objects
  (let* ((attrs (make-hash "type" "TestContact"))
         (hash  (make-hash "attributes" attrs
                           "Id"        "003xx"
                           "FirstName" "Alan"))
         (obj   (cl-sf/objects:hash->sobject hash)))
    (of-type test-contact obj "Type inferred from attributes.type")
    (is string= "Alan" (test-contact-first-name obj))))

(define-test hash->sobject/nil-value-not-set
  :parent objects
  ;; JSON null → CL nil → slot stays at initform (nil)
  (let* ((hash (make-hash "Id" "003xx" "FirstName" "Ada" "Email" nil))
         (obj  (cl-sf/objects:hash->sobject hash "TestContact")))
    (is string= "Ada" (test-contact-first-name obj))
    (false (test-contact-email obj) "nil value → slot stays nil")))

(define-test hash->sobject/unknown-type
  :parent objects
  (let* ((hash (make-hash "Id" "a01" "Name" "Widget"))
         (obj  (cl-sf/objects:hash->sobject hash "UnknownType__c")))
    (of-type cl-sf/objects:sobject obj "Falls back to plain sobject")
    (is string= "a01"            (cl-sf/objects:sobject-id  obj))
    (is string= "UnknownType__c" (cl-sf/objects:sobject-type obj))
    (true (cl-sf/objects:sobject-raw obj)
          "Raw hash preserved in sf-raw")))

(define-test hash->sobject/nil-hash
  :parent objects
  (false (cl-sf/objects:hash->sobject nil)
         "nil hash → returns nil"))

(define-test hash->sobject/raw-preserved
  :parent objects
  ;; sf-raw is always set on typed instances too
  (let* ((hash (make-hash "Id" "003xx" "FirstName" "Linus"))
         (obj  (cl-sf/objects:hash->sobject hash "TestContact")))
    (is eq hash (cl-sf/objects:sobject-raw obj)
        "Original hash stored in sf-raw for all-field access")))

;;; ──────────────────────────────────────────────────────────────────
;;; sobject->hash-table
;;; ──────────────────────────────────────────────────────────────────

(define-test sobject->hash-table/non-nil-slots-included
  :parent objects
  (let* ((obj  (make-instance 'test-contact
                              :first-name "Nikola"
                              :last-name  "Tesla"))
         (hash (cl-sf/objects:sobject->hash-table obj)))
    (is string= "Nikola" (gethash "FirstName" hash))
    (is string= "Tesla"  (gethash "LastName"  hash))))

(define-test sobject->hash-table/nil-slots-omitted
  :parent objects
  (let* ((obj  (make-instance 'test-contact :first-name "Only"))
         (hash (cl-sf/objects:sobject->hash-table obj)))
    (true  (gethash "FirstName" hash)   "FirstName present (non-nil)")
    (false (nth-value 1 (gethash "Email" hash))
           "Email absent from hash (nil slot)"))
  ;; Note: (gethash "Email" hash) returns (values nil nil) when key absent
  ;; We need the secondary value to distinguish absent from nil-valued key
  )

(define-test sobject->hash-table/id-excluded
  :parent objects
  ;; Id is in *sf-read-only-fields* and must NEVER be sent in create/update
  (let* ((obj  (make-instance 'test-contact :id "003xx" :first-name "Marie"))
         (hash (cl-sf/objects:sobject->hash-table obj)))
    (false (nth-value 1 (gethash "Id" hash))
           "Id excluded from serialized hash")))

(define-test sobject->hash-table/read-only-fields-excluded
  :parent objects
  (dolist (field cl-sf/objects::*sf-read-only-fields*)
    (let* ((obj  (make-instance 'cl-sf/objects:sobject
                                :raw (make-hash field "some-value" "Name" "Test")))
           (hash (cl-sf/objects:sobject->hash-table obj)))
      (false (nth-value 1 (gethash field hash))
             (format nil "Read-only field ~A excluded" field)))))

(define-test sobject->hash-table/generic-copies-raw
  :parent objects
  (let* ((raw  (make-hash "Name" "Generic Obj" "Custom__c" "val"))
         (obj  (make-instance 'cl-sf/objects:sobject :type "Custom__c" :raw raw))
         (hash (cl-sf/objects:sobject->hash-table obj)))
    (is string= "Generic Obj" (gethash "Name"      hash))
    (is string= "val"         (gethash "Custom__c"  hash))))

(define-test sobject->hash-table/round-trip
  :parent objects
  ;; Create a typed object, serialise it, reconstruct it — slots should match
  (let* ((orig (make-instance 'test-contact
                              :first-name "Hedy"
                              :last-name  "Lamarr"
                              :email      "hedy@example.com"))
         (hash (cl-sf/objects:sobject->hash-table orig))
         (back (cl-sf/objects:hash->sobject hash "TestContact")))
    (is string= "Hedy"              (test-contact-first-name back))
    (is string= "Lamarr"            (test-contact-last-name  back))
    (is string= "hedy@example.com"  (test-contact-email      back))))

;;; ──────────────────────────────────────────────────────────────────
;;; sobject-field-names
;;; ──────────────────────────────────────────────────────────────────

(define-test sobject-field-names/typed
  :parent objects
  (let* ((obj   (make-instance 'test-contact))
         (names (cl-sf/objects:sobject-field-names obj)))
    (true (listp names) "Returns a list")
    (true (member "FirstName"    names :test #'equal))
    (true (member "LastName"     names :test #'equal))
    (true (member "Email"        names :test #'equal))
    (true (member "AnnualSalary" names :test #'equal))))

(define-test sobject-field-names/generic-sobject
  :parent objects
  (let ((obj (make-instance 'cl-sf/objects:sobject)))
    (false (cl-sf/objects:sobject-field-names obj)
           "Generic SOBJECT → NIL (no registered slot map)")))

;;; ──────────────────────────────────────────────────────────────────
;;; print-object
;;; ──────────────────────────────────────────────────────────────────

(define-test print-object/includes-type-and-id
  :parent objects
  (let* ((obj (make-instance 'test-contact :id "003xx" :type "TestContact"))
         (str (with-output-to-string (s) (print-object obj s))))
    (true (cl-ppcre:scan "TestContact" str) "type name in printed output")
    (true (cl-ppcre:scan "003xx" str)       "Id in printed output")))

(define-test print-object/unsaved-object
  :parent objects
  (let* ((obj (make-instance 'cl-sf/objects:sobject :type "Account"))
         (str (with-output-to-string (s) (print-object obj s))))
    (true (cl-ppcre:scan "unsaved" str) "(unsaved) shown when no Id")))

(define-test print-object/shows-queried-fields
  :parent objects
  (let* ((obj (cl-sf/objects:hash->sobject (mock-record "Id" "001xx" "Name" "Acme")))
         (str (with-output-to-string (s) (print-object obj s))))
    (true (cl-ppcre:scan "^Account\\(" str)    "leads with Type(")
    (true (cl-ppcre:scan "Id=001xx" str)       "shows queried Id field")
    (true (cl-ppcre:scan "Name=Acme" str)      "shows queried Name field")
    (false (cl-ppcre:scan "#<" str)             "no #<...> wrapper")))

(define-test print-object/relationship-field-prints-recursively
  :parent objects
  ;; Real Salesforce shape for a dot-path relationship field: a hash with
  ;; its own "attributes.type" and only the fields actually requested.
  (let* ((attrs      (make-hash-table :test #'equal))
         (created-by (make-hash-table :test #'equal))
         (raw        (mock-record "Id" "001xx" "Name" "Acme")))
    (setf (gethash "type" attrs) "User"
          (gethash "attributes" created-by) attrs
          (gethash "Name" created-by) "Grace Hopper"
          (gethash "CreatedBy" raw) created-by)
    (let* ((obj (cl-sf/objects:hash->sobject raw))
           (str (with-output-to-string (s) (print-object obj s))))
      (true (cl-ppcre:scan "CreatedBy=User\\(Name=Grace Hopper\\)" str)
            "relationship record prints exactly as it would standalone"))))

(define-test print-object/subquery-field-prints-each-record
  :parent objects
  ;; Real Salesforce shape for a child sub-query field: a wrapper hash
  ;; with totalSize/done/records, not a bare array.
  (let* ((wrapper (make-hash-table :test #'equal))
         (raw     (mock-record "Id" "001xx" "Name" "Acme")))
    (setf (gethash "totalSize" wrapper) 2
          (gethash "done" wrapper) t
          (gethash "records" wrapper)
          (vector (mock-record "Id" "003a" "LastName" "Hopper")
                  (mock-record "Id" "003b" "LastName" "Turing"))
          (gethash "Contacts" raw) wrapper)
    (let* ((obj (cl-sf/objects:hash->sobject raw))
           (str (with-output-to-string (s) (print-object obj s))))
      (true (cl-ppcre:scan "Contacts=\\[.*LastName=Hopper.*LastName=Turing.*\\]" str)
            "each sub-query record prints exactly as it would standalone"))))

;;; ──────────────────────────────────────────────────────────────────
;;; SOQL DSL — IN-list value forms, FIELDS escape, SELECT * wildcard
;;; ──────────────────────────────────────────────────────────────────

(define-test soql/in-quoted-literal-list
  :parent objects
  (is string= "SELECT ID FROM ACCOUNT WHERE STATUS IN ('A', 'B')"
      (cl-sf/objects:soql select id from account where (in status '("A" "B")))))

(define-test soql/in-bound-variable-list
  :parent objects
  (let ((statuses (list "A" "B")))
    (is string= "SELECT ID FROM ACCOUNT WHERE STATUS IN ('A', 'B')"
        (cl-sf/objects:soql select id from account where (in status statuses)))))

(define-test soql/in-call-form-list
  :parent objects
  (is string= "SELECT ID FROM ACCOUNT WHERE STATUS IN ('A', 'B')"
      (cl-sf/objects:soql select id from account where (in status (list "A" "B")))))

(define-test soql/fields-runtime-escape
  :parent objects
  (let ((cols (list "Id" "Name")))
    (is string= "SELECT Id, Name FROM ACCOUNT"
        (cl-sf/objects:soql select (fields cols) from account))))

(define-test soql/select-wildcard-uses-cached-fields
  :parent objects
  ;; Network-free: pre-seed the wildcard cache so DESCRIBE-SOBJECT (which
  ;; needs a live CONN) is never called — proves the cache hit path works.
  (setf (gethash "WILDCARDTESTTYPE" cl-sf/objects::*wildcard-field-names-cache*)
        (list "Id" "Name"))
  (is string= "SELECT Id, Name FROM WILDCARDTESTTYPE"
      (cl-sf/objects:soql select * from wildcardtesttype)))
