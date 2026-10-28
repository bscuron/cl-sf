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
;;; SELECT / SOQL — infix SOQL syntax compiled to text
;;; ──────────────────────────────────────────────────────────────────

(defmacro soql= (expected &body clauses)
  `(is string= ,expected (cl-sf/objects:soql ,@clauses)))

(define-test soql/basic-clauses
  :parent objects
  (soql= "SELECT ID, NAME FROM ACCOUNT"
         select id name from account)
  (soql= "SELECT ID FROM ACCOUNT WHERE NAME = 'Acme' ORDER BY NAME DESC NULLS LAST, ID ASC LIMIT 10 OFFSET 5"
         select id from account where name = "Acme"
         order by name desc nulls last id asc limit 10 offset 5)
  (soql= "SELECT ID FROM ACCOUNT FOR UPDATE"
         select id from account for update)
  (soql= "SELECT ID FROM ACCOUNT FOR VIEW UPDATE TRACKING"
         select id from account for view update tracking))

(define-test soql/where-boolean-logic
  :parent objects
  (soql= "SELECT ID FROM ACCOUNT WHERE (A = 1 AND B > 2)"
         select id from account where a = 1 and b > 2)
  (soql= "SELECT ID FROM ACCOUNT WHERE (A = 1 OR (B = 2 AND C = 3))"
         select id from account where a = 1 or b = 2 and c = 3)
  (soql= "SELECT ID FROM ACCOUNT WHERE ((A = 1 OR B = 2) AND NOT C LIKE 'x%')"
         select id from account where (a = 1 or b = 2) and not c like "x%")
  (soql= "SELECT ID FROM ACCOUNT WHERE (A != NULL AND B <> TRUE)"
         select id from account where a != null and b <> true))

(define-test soql/value-forms
  :parent objects
  (let ((name "O'Brien") (n 3))
    (soql= "SELECT ID FROM ACCOUNT WHERE NAME = 'O\\'Brien'"
           select id from account where name = name)
    (soql= "SELECT ID FROM ACCOUNT WHERE (N > 6 AND M = 'x')"
           select id from account where n > (* n 2) and m = "x")
    (soql= "SELECT ID FROM ACCOUNT LIMIT 3"
           select id from account limit n))
  (soql= "SELECT ID FROM ACCOUNT WHERE D = LAST_N_DAYS:5"
         select id from account where d = (raw "LAST_N_DAYS:5"))
  (soql= "SELECT ID FROM ACCOUNT WHERE X = 1.5"
         select id from account where x = 1.5))

(define-test soql/in-lists
  :parent objects
  (let ((statuses (list "A" "B")))
    (soql= "SELECT ID FROM ACCOUNT WHERE STATUS IN ('A', 'B')"
           select id from account where status in ("A" "B"))
    (soql= "SELECT ID FROM ACCOUNT WHERE STATUS IN ('A', 'B')"
           select id from account where status in statuses)
    (soql= "SELECT ID FROM ACCOUNT WHERE STATUS NOT IN ('A', 'B')"
           select id from account where status not in (list "A" "B")))
  (soql= "SELECT ID FROM ACCOUNT WHERE ID IN (SELECT ACCOUNTID FROM CONTACT WHERE X = 1)"
         select id from account where id in (select accountid from contact where x = 1))
  (soql= "SELECT ID FROM ACCOUNT WHERE TAGS INCLUDES ('a', 'b')"
         select id from account where tags includes ("a" "b")))

(define-test soql/select-list-forms
  :parent objects
  (soql= "SELECT COUNT(ID) CNT, INDUSTRY FROM ACCOUNT GROUP BY INDUSTRY HAVING COUNT(ID) > 5"
         select (count id) cnt industry from account
         group by industry having (count id) > 5)
  (soql= "SELECT FIELDS(ALL) FROM ACCOUNT LIMIT 200"
         select (fields all) from account limit 200)
  (soql= "SELECT ID, (SELECT ID FROM CONTACTS) FROM ACCOUNT"
         select id (select id from contacts) from account)
  (soql= "SELECT TYPEOF WHAT WHEN ACCOUNT THEN PHONE, WEBSITE WHEN OPPORTUNITY THEN AMOUNT ELSE NAME END FROM EVENT"
         select typeof what when account then phone website
                when opportunity then amount else name end
         from event)
  (soql= "SELECT ID FROM ACCOUNT GROUP BY ROLLUP(A, B)"
         select id from account group by (rollup a b))
  (soql= "SELECT ID FROM ACCOUNT WHERE CALENDAR_YEAR(CLOSEDATE) = 2024"
         select id from account where (calendar_year closedate) = 2024))

(define-test soql/other-clauses
  :parent objects
  (soql= "SELECT ID FROM ACCOUNT USING SCOPE MINE WITH SECURITY_ENFORCED"
         select id from account using scope mine with security_enforced)
  (soql= "SELECT ID FROM ACCOUNT WITH DATA CATEGORY GEO__C AT (USA__C, UK__C)"
         select id from account with data category geo__c at (usa__c uk__c))
  (soql= "SELECT ID FROM ACCOUNT SET OPTIONS (A = 1, B = 'x')"
         select id from account set options (a = 1 b = "x")))

(define-test soql/object-named-like-a-clause
  :parent objects
  (soql= "SELECT ID FROM ORDER ORDER BY ID"
         select id from order order by id))

(define-test soql/malformed-queries-signal-errors
  :parent objects
  (fail (macroexpand-1 '(cl-sf/objects:soql select id from account where)))
  (fail (macroexpand-1 '(cl-sf/objects:soql select id from account where a ~ 1)))
  (fail (macroexpand-1 '(cl-sf/objects:soql select from account)))
  (fail (macroexpand-1 '(cl-sf/objects:soql id from account))))

(define-test select/expands-to-constant-text-when-static
  :parent objects
  (is equal '(cl-sf/objects::%select "SELECT ID FROM ACCOUNT" nil)
      (macroexpand-1 '(cl-sf/objects:select id from account)))
  (is equal '(cl-sf/objects::%select "SELECT ID FROM ACCOUNT" t)
      (macroexpand-1 '(cl-sf/objects:select id from account all rows))))

;;; ── %QUERY-RECORDS — eager pagination ─────────────────────────────

(defmacro with-stubbed-queries ((&key first next) &body body)
  "Run BODY with /query returning FIRST and every next-page URL returning
   the matching response from the alist NEXT."
  `(let ((orig-query (fdefinition 'cl-sf/api::%query))
         (orig-next  (fdefinition 'cl-sf/api::%query-next-page)))
     (unwind-protect
          (progn
            (setf (fdefinition 'cl-sf/api::%query) (lambda (conn soql) (declare (ignore conn soql)) ,first)
                  (fdefinition 'cl-sf/api::%query-next-page)
                  (lambda (conn url) (declare (ignore conn)) (cdr (assoc url ,next :test #'string=))))
            ,@body)
       (setf (fdefinition 'cl-sf/api::%query) orig-query
             (fdefinition 'cl-sf/api::%query-next-page) orig-next))))

(define-test select/follows-every-page-into-one-vector
  :parent objects
  (with-stubbed-queries
      (:first (mock-query-response :total 5 :done nil :records (vector 1 2) :next-url "p2")
       :next  (list (cons "p2" (mock-query-response :total 5 :done nil :records (vector 3 4) :next-url "p3"))
                    (cons "p3" (mock-query-response :total 5 :done t :records (vector 5)))))
    (let ((result (cl-sf/api::%query-records nil "q" :coercer #'1+)))
      (true (simple-vector-p result))
      (is equalp #(2 3 4 5 6) result))))

(define-test select/empty-result-is-empty-vector
  :parent objects
  (with-stubbed-queries (:first (mock-query-response :total 0))
    (is equalp #() (cl-sf/api::%query-records nil "q"))))

(define-test select/returns-typed-clos-objects
  :parent objects
  (flet ((rec (id last-name)
           (let ((h (make-hash-table :test #'equal))
                 (attrs (make-hash-table :test #'equal)))
             (setf (gethash "type" attrs) "TestContact"
                   (gethash "attributes" h) attrs
                   (gethash "Id" h) id
                   (gethash "LastName" h) last-name)
             h)))
    (with-stubbed-queries
        (:first (mock-query-response :total 2 :done t
                                     :records (vector (rec "003A" "Hopper") (rec "003B" "Turing"))))
      (let ((result (cl-sf/objects:select id last-name from testcontact)))
        (true (simple-vector-p result))
        (is = 2 (length result))
        (true (typep (aref result 0) 'test-contact))
        (is string= "Hopper" (test-contact-last-name (aref result 0)))
        (is string= "003B" (cl-sf/objects:sobject-id (aref result 1)))))))
