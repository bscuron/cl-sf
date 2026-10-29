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


;;; ── DML routing: single REST call / Collections API / Bulk ────────

(defvar *dml-log-lock* (sb-thread:make-mutex :name "test log"))

(defmacro log-push (item log)
  `(sb-thread:with-mutex (*dml-log-lock*) (push ,item ,log)))

(defmacro with-stubbed-dml ((log) &body body)
  "Replace the single-record REST helpers, the Collections DML step and the
   bulk entry points with stubs that push (kind . detail) onto LOG."
  `(let ((saved (mapcar #'fdefinition
                        '(cl-sf/objects::%insert-one cl-sf/objects::%delete-one
                          cl-sf/objects::%collection-dml
                          cl-sf/bulk:bulk-insert cl-sf/bulk:bulk-delete))))
     (unwind-protect
          (progn
            (setf (fdefinition 'cl-sf/objects::%insert-one)
                  (lambda (o conn) (declare (ignore conn)) (log-push (cons :rest o) ,log) o)
                  (fdefinition 'cl-sf/objects::%delete-one)
                  (lambda (o conn) (declare (ignore conn))
                    (log-push (list :rest-delete (cl-sf/objects:sobject-type o)
                                    (cl-sf/objects:sobject-id o))
                              ,log)
                    t)
                  (fdefinition 'cl-sf/objects::%collection-dml)
                  (lambda (op objects conn &key external-id-field)
                    (declare (ignore conn))
                    (log-push (list :collection op (length objects)
                                    (cl-sf/objects:sobject-type (first objects))
                                    external-id-field)
                              ,log)
                    objects)
                  (fdefinition 'cl-sf/bulk:bulk-insert)
                  (lambda (objs &key conn &allow-other-keys) (declare (ignore conn))
                    (log-push (list :bulk :insert (length objs)) ,log) :bulk-result)
                  (fdefinition 'cl-sf/bulk:bulk-delete)
                  (lambda (objs &key conn &allow-other-keys) (declare (ignore conn))
                    (log-push (list :bulk :delete (length objs)) ,log) :bulk-result))
            ,@body)
       (loop for name in '(cl-sf/objects::%insert-one cl-sf/objects::%delete-one
                           cl-sf/objects::%collection-dml
                           cl-sf/bulk:bulk-insert cl-sf/bulk:bulk-delete)
             for fn in saved
             do (setf (fdefinition name) fn)))))

(defmacro with-key-prefixes ((conn-var &rest prefixes) &body body)
  "Run BODY with CONN-VAR bound to a fresh connection whose org maps each
   (PREFIX NAME) pair, via a stubbed DESCRIBE-GLOBAL."
  `(let* ((,conn-var (cl-sf/auth:make-connection
                      :instance-url (format nil "https://~A.example.com" (random most-positive-fixnum))))
          (orig (fdefinition 'cl-sf/api::%describe-global)))
     (unwind-protect
          (progn
            (setf (fdefinition 'cl-sf/api::%describe-global)
                  (lambda (conn) (declare (ignore conn))
                    (let ((h (make-hash-table :test #'equal)))
                      (setf (gethash "sobjects" h)
                            (map 'vector (lambda (p)
                                           (let ((o (make-hash-table :test #'equal)))
                                             (setf (gethash "keyPrefix" o) (first p)
                                                   (gethash "name" o) (second p))
                                             o))
                                 ',prefixes))
                      h)))
            ,@body)
       (setf (fdefinition 'cl-sf/api::%describe-global) orig))))

(defun contacts (n)
  (loop for i below n
        collect (make-instance 'test-contact :last-name (format nil "~D" i)
                                             :id (format nil "003~12,'0D" i))))

(define-test database-insert/routes-by-count
  :parent objects
  (let ((log '()) (objs (contacts 3)))
    (with-stubbed-dml (log)
      ;; one object -> a single REST call, returns the object
      (is eq (first objs) (cl-sf/objects:database-insert (first objs)))
      (is equal :rest (car (first log)))
      ;; a sequence at or below the threshold -> Collections, returns the sequence
      (setf log '())
      (is eq objs (cl-sf/objects:database-insert objs :bulk-threshold 3))
      (is equal '((:collection :insert 3 "TestContact" nil)) log)
      ;; above the threshold -> one bulk call
      (setf log '())
      (is eq :bulk-result (cl-sf/objects:database-insert objs :bulk-threshold 2))
      (is equal '((:bulk :insert 3)) log)
      ;; vectors work; 0 always bulk; NIL never bulk
      (setf log '())
      (cl-sf/objects:database-insert (coerce objs 'vector) :bulk-threshold 0)
      (is equal '((:bulk :insert 3)) log)
      (setf log '())
      (cl-sf/objects:database-insert (coerce objs 'vector) :bulk-threshold nil)
      (is equal '((:collection :insert 3 "TestContact" nil)) log)
      ;; an empty sequence sends nothing
      (setf log '())
      (cl-sf/objects:database-insert '())
      (is equal '() log))))

(define-test database-upsert-and-update/pass-through-the-operation
  :parent objects
  (let ((log '()) (objs (contacts 2)))
    (with-stubbed-dml (log)
      (cl-sf/objects:database-upsert objs "Ext__c")
      (cl-sf/objects:database-update objs)
      (is equal '((:collection :update 2 "TestContact" nil)
                  (:collection :upsert 2 "TestContact" "Ext__c"))
          log))))

(define-test sobject-type-of-id/uses-key-prefix
  :parent objects
  (with-key-prefixes (conn ("001" "Account") ("003" "Contact") ("a0B" "Thing__c"))
    (is string= "Account" (cl-sf/objects::%sobject-type-of-id "001xx000003DGb2AAG" conn))
    (is string= "Contact" (cl-sf/objects::%sobject-type-of-id "003xx000004TmiQ" conn))
    (is string= "Thing__c" (cl-sf/objects::%sobject-type-of-id "a0Bxx0000012345" conn))
    (fail (cl-sf/objects::%sobject-type-of-id "zzzxx000003DGb2AAG" conn) 'cl-sf/api:sf-api-error)
    (fail (cl-sf/objects::%sobject-type-of-id "001" conn) 'cl-sf/api:sf-api-error)))

(define-test database-delete/infers-missing-type-from-id
  :parent objects
  (with-key-prefixes (conn ("001" "Account") ("003" "Contact"))
    (let ((log '())
          (untyped (make-instance 'cl-sf/objects:sobject :id "001xx000003DGb2AAG")))
      (with-stubbed-dml (log)
        (is eq t (cl-sf/objects:database-delete untyped :conn conn))
        (is equal '(:rest-delete "Account" "001xx000003DGb2AAG") (first log))
        ;; the inferred type is stored on the object
        (is string= "Account" (cl-sf/objects:sobject-type untyped))
        (setf log '())
        (is eq t (cl-sf/objects:database-delete (list untyped) :conn conn))
        (is equal '((:collection :delete 1 "Account" nil)) log)))))

(define-test database-delete/routes-and-returns
  :parent objects
  (let ((log '()) (objs (contacts 3)))
    (with-stubbed-dml (log)
      (is eq t (cl-sf/objects:database-delete objs))
      (is eq t (cl-sf/objects:database-delete (first objs)))
      (is eq :bulk-result (cl-sf/objects:database-delete objs :bulk-threshold 1))
      (is equal '(:bulk :delete 3) (first log)))))

(define-test database-calls/mixed-types-are-an-error
  :parent objects
  (with-key-prefixes (conn ("001" "Account") ("003" "Contact"))
    (let ((log '())
          (a (make-instance 'cl-sf/objects:sobject :id "001xx000003DGb2AAG" :type "Account"))
          (c (make-instance 'test-contact :id "003xx000004TmiQAAS")))
      (with-stubbed-dml (log)
        (fail (cl-sf/objects:database-insert (list a c) :conn conn) 'cl-sf/api:sf-api-error)
        (fail (cl-sf/objects:database-update (vector a c) :conn conn) 'cl-sf/api:sf-api-error)
        (fail (cl-sf/objects:database-upsert (list a c) "Ext__c" :conn conn) 'cl-sf/api:sf-api-error)
        (fail (cl-sf/objects:database-delete (list a c) :conn conn :bulk-threshold 0)
              'cl-sf/api:sf-api-error)
        (is equal '() log)))))

(define-test database-calls/non-sobjects-are-an-sf-api-error
  :parent objects
  (let ((log '()))
    (with-stubbed-dml (log)
      (fail (cl-sf/objects:database-delete "001xx000003DGb2AAG") 'cl-sf/api:sf-api-error)
      (fail (cl-sf/objects:database-delete 42) 'cl-sf/api:sf-api-error)
      (fail (cl-sf/objects:database-delete (list "001xx000003DGb2AAG")) 'cl-sf/api:sf-api-error)
      (fail (cl-sf/objects:database-insert (list (make-instance 'test-contact) :oops))
            'cl-sf/api:sf-api-error)
      (is equal '() log))))

(define-test database-delete/sobject-without-id-is-an-error
  :parent objects
  (fail (cl-sf/objects:database-delete (make-instance 'test-contact)) 'cl-sf/api:sf-api-error)
  (fail (cl-sf/objects:database-delete (list (make-instance 'test-contact))) 'cl-sf/api:sf-api-error))

;;; ── The Collections API step itself ───────────────────────────────

(defun last-name-of (r)
  (if (hash-table-p r) (gethash "LastName" r) r))

(defmacro with-stubbed-collection-api ((log &key fail-names fail-request-containing) &body body)
  "Stub the Collections HTTP calls.  Each logs (op . record-count) and answers
   per record: success with id ID-<LastName>, or failure when the record's
   LastName is in FAIL-NAMES.  A request containing FAIL-REQUEST-CONTAINING
   signals SF-API-ERROR as a whole."
  `(let* ((names '(cl-sf/api::%collection-create cl-sf/api::%collection-update
                   cl-sf/api::%collection-upsert cl-sf/api::%collection-delete))
          (saved (mapcar #'fdefinition names)))
     (unwind-protect
          (flet ((answer (op records)
                   (log-push (cons op (length records)) ,log)
                   (when (and ,fail-request-containing
                              (find ,fail-request-containing records
                                    :key #'last-name-of :test #'equal))
                     (error 'cl-sf/api:sf-api-error :message "request rejected" :http-status 400))
                   (map 'vector
                        (lambda (r)
                          (let ((name (last-name-of r))
                                (h (make-hash-table :test #'equal)))
                            (cond ((member name ,fail-names :test #'equal)
                                   (let ((e (make-hash-table :test #'equal)))
                                     (setf (gethash "statusCode" e) "REQUIRED_FIELD_MISSING"
                                           (gethash "message" e) "Required fields are missing"
                                           (gethash "fields" e) (vector "LastName")
                                           (gethash "success" h) nil
                                           (gethash "errors" h) (vector e))))
                                  (t (setf (gethash "success" h) t
                                           (gethash "id" h) (format nil "ID-~A" name))))
                            h))
                        records)))
            (setf (fdefinition 'cl-sf/api::%collection-create)
                  (lambda (conn records) (declare (ignore conn)) (answer :create records))
                  (fdefinition 'cl-sf/api::%collection-update)
                  (lambda (conn records) (declare (ignore conn)) (answer :update records))
                  (fdefinition 'cl-sf/api::%collection-upsert)
                  (lambda (conn type field records) (declare (ignore conn))
                    (log-push (list :upsert-target type field) ,log)
                    (answer :upsert records))
                  (fdefinition 'cl-sf/api::%collection-delete)
                  (lambda (conn ids) (declare (ignore conn)) (answer :delete ids)))
            ,@body)
       (loop for name in names for fn in saved do (setf (fdefinition name) fn)))))

(defun numbered-contacts (n)
  (loop for i below n collect (make-instance 'test-contact :last-name (format nil "~D" i))))

(define-test collection-dml/splits-into-requests-of-200-and-sets-ids-in-order
  :parent objects
  (let ((log '()) (objs (numbered-contacts 450)))
    (with-stubbed-collection-api (log)
      (is eq objs (cl-sf/objects:database-insert objs :bulk-threshold nil))
      (is equal '(50 200 200) (sort (mapcar #'cdr log) #'<))
      (is equal (loop for i below 450 collect (format nil "ID-~D" i))
          (mapcar #'cl-sf/objects:sobject-id objs)))))

(define-test collection-dml/reports-failed-records-after-processing-everything
  :parent objects
  (let ((log '()) (objs (numbered-contacts 450)))
    (with-stubbed-collection-api (log :fail-names '("7" "300"))
      (let ((condition (handler-case (progn (cl-sf/objects:database-insert objs :bulk-threshold nil) nil)
                         (cl-sf/api:sf-dml-error (e) e))))
        (true condition)
        (is = 448 (cl-sf/api:sf-dml-error-succeeded condition))
        (is equal '("7" "300")
            (mapcar (lambda (f) (test-contact-last-name (car f)))
                    (cl-sf/api:sf-dml-error-failures condition)))
        (true (search "REQUIRED_FIELD_MISSING: Required fields are missing (LastName)"
                      (cdr (first (cl-sf/api:sf-dml-error-failures condition)))))
        (true (search "2 of 450 records failed" (cl-sf/api:sf-api-error-message condition)))
        ;; every chunk was still sent, and the successes got their Ids
        (is = 3 (length log))
        (is string= "ID-8" (cl-sf/objects:sobject-id (nth 8 objs)))
        (false (cl-sf/objects:sobject-id (nth 7 objs)))))))

(define-test collection-dml/a-rejected-request-fails-only-its-own-records
  :parent objects
  (let ((log '()) (objs (numbered-contacts 450)))
    (with-stubbed-collection-api (log :fail-request-containing "250")
      (let ((condition (handler-case (progn (cl-sf/objects:database-insert objs :bulk-threshold nil) nil)
                         (cl-sf/api:sf-dml-error (e) e))))
        (is = 250 (cl-sf/api:sf-dml-error-succeeded condition))
        (is = 200 (length (cl-sf/api:sf-dml-error-failures condition)))
        (true (search "request rejected" (cdr (first (cl-sf/api:sf-dml-error-failures condition)))))
        (is string= "ID-0" (cl-sf/objects:sobject-id (first objs)))
        (false (cl-sf/objects:sobject-id (nth 250 objs)))))))

(define-test collection-dml/update-and-delete-need-ids-before-sending-anything
  :parent objects
  (let ((log '()) (objs (list (make-instance 'test-contact :last-name "a" :id "003A")
                              (make-instance 'test-contact :last-name "b"))))
    (with-stubbed-collection-api (log)
      (fail (cl-sf/objects:database-update objs) 'cl-sf/api:sf-api-error)
      (fail (cl-sf/objects:database-delete objs) 'cl-sf/api:sf-api-error)
      (is equal '() log))))

(define-test collection-dml/request-bodies
  :parent objects
  (let ((rec (cl-sf/objects::%collection-record
              (make-instance 'test-contact :last-name "Hopper" :id "003X") "TestContact" :id "003X")))
    (is string= "TestContact" (gethash "type" (gethash "attributes" rec)))
    (is string= "Hopper" (gethash "LastName" rec))
    (is string= "003X" (gethash "Id" rec)))
  (false (gethash "Id" (cl-sf/objects::%collection-record
                        (make-instance 'test-contact :id "003X") "TestContact")))
  (let ((body (cl-sf/api::%collection-body (list (make-hash-table)))))
    (is equal nil (gethash "allOrNone" body))
    (true (simple-vector-p (gethash "records" body)))))

(define-test collection-dml/upsert-targets-type-and-external-id-field
  :parent objects
  (let ((log '()) (objs (numbered-contacts 3)))
    (with-stubbed-collection-api (log)
      (cl-sf/objects:database-upsert objs "Ext__c")
      (true (member '(:upsert-target "TestContact" "Ext__c") log :test #'equal)))))

;;; ── sobject->hash-table caches ────────────────────────────────────

(cl-sf/objects:define-sobject-class cache-probe "CacheProbe"
  (alpha "Alpha__c")
  (beta  "Beta__c")
  (gamma "Gamma__c"))

(define-test sobject->hash-table/unbound-and-nil-slots-are-omitted
  :parent objects
  (let ((o (make-instance 'cache-probe :alpha "a" :beta nil)))
    (slot-makunbound o 'gamma)
    (let ((h (cl-sf/objects:sobject->hash-table o)))
      (is = 1 (hash-table-count h))
      (is string= "a" (gethash "Alpha__c" h)))))

(define-test sobject->hash-table/follows-changes-to-the-read-only-list
  :parent objects
  (let ((o (make-instance 'cache-probe :alpha "a" :beta "b")))
    (is = 2 (hash-table-count (cl-sf/objects:sobject->hash-table o)))   ; primes the cache
    (let ((cl-sf/objects:*sf-read-only-fields* (cons "Beta__c" cl-sf/objects:*sf-read-only-fields*)))
      (let ((h (cl-sf/objects:sobject->hash-table o)))
        (is = 1 (hash-table-count h))
        (false (gethash "Beta__c" h))))
    ;; and back again once the binding ends
    (is = 2 (hash-table-count (cl-sf/objects:sobject->hash-table o)))))

(define-test sobject->hash-table/follows-class-redefinition
  :parent objects
  (let ((o (make-instance 'cache-probe :alpha "a" :beta "b")))
    (is = 2 (hash-table-count (cl-sf/objects:sobject->hash-table o)))
    (unwind-protect
         (progn
           (cl-sf/objects:define-sobject-class cache-probe "CacheProbe"
             (alpha "Alpha__c"))                       ; beta and gamma are gone
           (let ((h (cl-sf/objects:sobject->hash-table (make-instance 'cache-probe :alpha "a"))))
             (is = 1 (hash-table-count h))))
      (cl-sf/objects:define-sobject-class cache-probe "CacheProbe"
        (alpha "Alpha__c")
        (beta  "Beta__c")
        (gamma "Gamma__c")))))

(define-test sobject->hash-table/generic-skips-read-only-keys
  :parent objects
  (let ((raw (make-hash-table :test #'equal)))
    (setf (gethash "Name" raw) "n" (gethash "Id" raw) "x" (gethash "CreatedDate" raw) "d"
          (gethash "attributes" raw) "a")
    (let ((h (cl-sf/objects:sobject->hash-table
              (make-instance 'cl-sf/objects:sobject :raw raw :type "X"))))
      (is = 1 (hash-table-count h))
      (is string= "n" (gethash "Name" h)))
    ;; no raw data at all: an empty table, not an error
    (is = 0 (hash-table-count
             (cl-sf/objects:sobject->hash-table (make-instance 'cl-sf/objects:sobject :type "X"))))))
