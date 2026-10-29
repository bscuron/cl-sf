;;;; cl-sf/tests/graphql-tests.lisp
;;;; GQL — query text, name resolution, paging and result shape.
;;;; Describe metadata and HTTP are stubbed; nothing touches the network.

(in-package #:cl-sf/tests)

(define-test graphql
  :parent cl-sf/tests
  :description "gql — s-expression GraphQL front end.")

;;; ── Fixture org ───────────────────────────────────────────────────

(defun gql-describe (name &key fields parents children)
  "A describe result: FIELDS plain field names; PARENTS (field relationship
   . referenceTo...) lookups; CHILDREN (relationship . child-sObject)."
  (let ((d (make-hash-table :test #'equal)))
    (flet ((field (spec &optional rel refs)
             (let ((f (make-hash-table :test #'equal))
                   (n (if (consp spec) (car spec) spec)))
               (setf (gethash "name" f) n
                     (gethash "type" f) (if (consp spec) (cdr spec) "string")
                     (gethash "relationshipName" f) (or rel 'null)
                     (gethash "referenceTo" f) (coerce refs 'vector))
               f))
           (child (rel sobject)
             (let ((c (make-hash-table :test #'equal)))
               (setf (gethash "relationshipName" c) rel
                     (gethash "childSObject" c) sobject)
               c)))
      (setf (gethash "name" d) name
            (gethash "fields" d)
            (coerce (append (mapcar #'field fields)
                            (mapcar (lambda (p) (field (first p) (second p) (cddr p))) parents))
                    'vector)
            (gethash "childRelationships" d)
            (coerce (mapcar (lambda (c) (child (car c) (cdr c))) children) 'vector)))
    d))

(defparameter *gql-org*
  (list (gql-describe "Account"
                      :fields '("Id" "Name" "Industry" "AnnualRevenue" "My_Field__c" "Active__c"
                                ("CreatedDate" . "datetime"))
                      :parents '(("OwnerId" "Owner" "User") ("ParentId" "Parent" "Account"))
                      :children '(("Contacts" . "Contact") ("Opportunities" . "Opportunity")))
        (gql-describe "Contact"
                      :fields '("Id" "LastName" "Email")
                      :parents '(("AccountId" "Account" "Account")))
        (gql-describe "Opportunity" :fields '("Id" "Name" ("CloseDate" . "date") ("Amount" . "currency")))
        (gql-describe "User" :fields '("Id" "Name" "Email"))
        (gql-describe "Group" :fields '("Id" "Name"))
        (gql-describe "Lead" :fields '("Id" "Company")
                             :parents '(("OwnerId" "Owner" "Group" "User")))
        (gql-describe "GqlThing__c" :fields '("Id" "Name" "Type" "Score__c" "CreatedById"
                                              ("Seen__c" . "datetime") ("Born__c" . "date")
                                              ("CreatedDate" . "datetime"))
                                    :parents '(("OwnerId" "Owner" "User"))
                                    :children '(("Notes__r" . "GqlNote__c")))
        (gql-describe "GqlNote__c" :fields '("Id" "Name"))))

(defun json (string) (com.inuoe.jzon:parse string))

(defmacro with-gql-org ((requests-var &key responses (describes (gensym "D"))) &body body)
  "Run BODY on a fresh connection to a stubbed org: DESCRIBE-SOBJECT answers
   from *GQL-ORG* (counting calls in DESCRIBES), API-REQUEST records each
   GraphQL query text onto REQUESTS-VAR and answers from RESPONSES (JSON
   strings, in order).  Class registries are isolated."
  `(let* ((,requests-var '())
          (,describes 0)
          (answers (mapcar #'json ,responses))
          (cl-sf/auth:*connection*
            (cl-sf/auth:make-connection
             :instance-url (format nil "https://~A.example.com" (random most-positive-fixnum))))
          (cl-sf/objects:*sobject-classes* (make-hash-table :test #'equal))
          (cl-sf/objects:*sobject-slot-maps* (make-hash-table :test #'eq)))
     (declare (ignorable ,describes))
     (with-fdefinitions
         ((cl-sf/objects:describe-sobject
           (lambda (type &key conn)
             (declare (ignore conn))
             (incf ,describes)
             (or (find type *gql-org* :key (lambda (d) (gethash "name" d)) :test #'string-equal)
                 (error 'cl-sf/api:sf-api-error :message "NOT_FOUND"))))
          (cl-sf/api:api-request
           (lambda (method path &key conn body params content-type raw)
             (declare (ignore conn params content-type raw))
             (push (list method path (gethash "query" body)) ,requests-var)
             (or (pop answers) (error "unexpected request")))))
       ,@body)))

(defun query-of (request) (third request))

(defmacro gql-text (&body roots)
  "The query text `gql` sends for ROOTS (no response needed)."
  `(let ((requests '()))
     (with-gql-org (reqs :responses (list "{\"data\":{\"uiapi\":{\"query\":{}}}}"))
       (ignore-errors (cl-sf/objects:gql ,@roots))
       (setf requests reqs))
     (query-of (first requests))))

(defun wrap (root)
  (format nil "query { uiapi { query { ~A } } }" root))

(defparameter *tail* "pageInfo { hasNextPage endCursor } }")

;;; ── Query text ────────────────────────────────────────────────────

(define-test gql/basic-query-text
  :parent graphql
  (is string=
      (wrap (format nil "Account(where: {Industry: {in: [\"Tech\", \"Finance\"]}}, orderBy: {Name: {order: DESC}}, first: 10) { edges { node { Id Name { value } } } ~A" *tail*))
      (gql-text (Account :where (Industry (in "Tech" "Finance"))
                         :order-by (Name :desc)
                         :first 10
                  (edges (node Id (Name value)))))))

(define-test gql/no-first-fetches-everything-in-pages
  :parent graphql
  (is string=
      (wrap (format nil "Account(first: 2000, upperBound: 100000) { edges { node { Id } } ~A" *tail*))
      (gql-text (Account (edges (node Id))))))

(define-test gql/names-are-recovered-from-metadata-in-any-case
  :parent graphql
  (is string=
      (wrap (format nil "Account(where: {AnnualRevenue: {gt: 5}}, first: 2000, upperBound: 100000) { edges { node { Id My_Field__c { value } Owner { Name { value } } } } ~A" *tail*))
      (gql-text (account :where (annualrevenue (gt 5))
                  (edges (node id (my_field__c value) (owner (name value))))))))

(define-test gql/a-string-name-is-used-exactly-as-written
  :parent graphql
  (is string=
      (wrap (format nil "Account(first: 2000, upperBound: 100000) { edges { node { Id Whatever__c { value } } } ~A" *tail*))
      (gql-text (Account (edges (node Id ("Whatever__c" value)))))))

(define-test gql/alias
  :parent graphql
  (is string=
      (wrap (format nil "big: Account(first: 2000, upperBound: 100000) { edges { node { Id } } ~A" *tail*))
      (gql-text (Account :as "big" (edges (node Id))))))

(define-test gql/children-take-their-own-arguments
  :parent graphql
  (is string=
      (wrap (format nil "Account(first: 2000, upperBound: 100000) { edges { node { Name { value } Contacts(first: 5, where: {Email: {like: \"%@x.com\"}}, orderBy: {LastName: {order: ASC}}) { edges { node { LastName { value } } } } } } ~A" *tail*))
      (gql-text (Account (edges (node (Name value)
                                  (Contacts :first 5
                                            :where (Email (like "%@x.com"))
                                            :order-by (LastName :asc)
                                    (edges (node (LastName value))))))))))

(define-test gql/polymorphic-lookups-need-inline-fragments
  :parent graphql
  (is string=
      (wrap (format nil "Lead(first: 2000, upperBound: 100000) { edges { node { Owner { ... on User { Name { value } } ... on Group { Name { value } } __typename } } } ~A" *tail*))
      (gql-text (Lead (edges (node (Owner (:on User (Name value))
                                          (:on Group (Name value))
                                          __typename)))))))

(define-test gql/polymorphic-lookup-without-a-type-is-an-error-and-sends-nothing
  :parent graphql
  (with-gql-org (requests)
    (fail (cl-sf/objects:gql (Lead (edges (node (Owner (Name value)))))) 'cl-sf/api:sf-api-error)
    (is equal '() requests)))

(define-test gql/order-by-forms
  :parent graphql
  (is string=
      (wrap (format nil "Account(orderBy: {Name: {order: DESC, nulls: LAST}}, first: 2000, upperBound: 100000) { edges { node { Id } } ~A" *tail*))
      (gql-text (Account :order-by (Name :desc :nulls :last) (edges (node Id)))))
  (is string=
      (wrap (format nil "Account(orderBy: {Name: {order: DESC}, Id: {order: ASC}}, first: 2000, upperBound: 100000) { edges { node { Id } } ~A" *tail*))
      (gql-text (Account :order-by ((Name :desc) (Id :asc)) (edges (node Id)))))
  (is string=
      (wrap (format nil "Account(orderBy: {Name: {order: ASC}}, first: 2000, upperBound: 100000) { edges { node { Id } } ~A" *tail*))
      (gql-text (Account :order-by (Name (order :asc)) (edges (node Id))))))

(defmacro where-text (filter)
  `(let ((text (gql-text (Account :where ,filter (edges (node Id))))))
     (subseq text (+ (length "query { uiapi { query { Account(where: ") 0)
             (search ", first: 2000, upperBound: 100000)" text))))

(define-test gql/filters
  :parent graphql
  (is string= "{Industry: {eq: \"Tech\"}}" (where-text (Industry (eq "Tech"))))
  (is string= "{AnnualRevenue: {gte: 100, lt: 500}}" (where-text (AnnualRevenue (gte 100) (lt 500))))
  (is string= "{and: [{Industry: {eq: \"a\"}}, {or: [{Name: {like: \"A%\"}}, {not: {Name: {eq: \"B\"}}}]}]}"
      (where-text (and (Industry (eq "a"))
                       (or (Name (like "A%"))
                           (not (Name (eq "B")))))))
  (is string= "{Active__c: {eq: false}}" (where-text (Active__c (eq false))))
  (is string= "{Active__c: {eq: true}}" (where-text (Active__c (eq t))))
  (is string= "{Name: {eq: null}}" (where-text (Name (eq nil))))
  (is string= "{Industry: {in: [\"only\"]}}" (where-text (Industry (in "only")))))

(define-test gql/filter-through-a-parent
  :parent graphql
  (is string=
      (wrap (format nil "Contact(where: {Account: {Industry: {eq: \"Tech\"}}}, first: 2000, upperBound: 100000) { edges { node { Id } } ~A" *tail*))
      (gql-text (Contact :where (Account (Industry (eq "Tech"))) (edges (node Id))))))

(define-test gql/values-come-from-the-surrounding-code-and-are-escaped
  :parent graphql
  (let ((statuses (list "A" "B")) (floor 1000) (hostile "x\" } } evil") (none '()))
    (is string= "{Industry: {in: [\"A\", \"B\"]}}" (where-text (Industry (in statuses))))
    (is string= "{AnnualRevenue: {gt: 2000}}" (where-text (AnnualRevenue (gt (* 2 floor)))))
    (is string= "{Name: {eq: \"x\\\" } } evil\"}}" (where-text (Name (eq hostile))))
    (is string= "{Industry: {in: []}}" (where-text (Industry (in none))))
    (is string= "{Name: {eq: LAST_WEEK}}" (where-text (Name (eq (:raw "LAST_WEEK")))))))

(define-test gql/unknown-names-are-errors-before-anything-is-sent
  :parent graphql
  (with-gql-org (requests)
    (fail (cl-sf/objects:gql (Account (edges (node Nope)))) 'cl-sf/api:sf-api-error)
    (fail (cl-sf/objects:gql (Account :where (Nope (eq 1)) (edges (node Id)))) 'cl-sf/api:sf-api-error)
    (fail (cl-sf/objects:gql (Nonexistent (edges (node Id)))) 'cl-sf/api:sf-api-error)
    (is equal '() requests)))

(define-test gql/describe-metadata-is-fetched-once-per-type-and-org
  :parent graphql
  (let ((describes 0))
    (with-gql-org (requests :describes describes
                            :responses (list "{\"data\":{\"uiapi\":{\"query\":{\"Account\":{\"edges\":[]}}}}}"
                                             "{\"data\":{\"uiapi\":{\"query\":{\"Account\":{\"edges\":[]}}}}}"))
      (cl-sf/objects:gql (Account (edges (node Id (Owner (Name value))))))
      (is = 2 describes "Account and User")
      (cl-sf/objects:gql (Account (edges (node Id (Owner (Name value))))))
      (is = 2 describes "second query: all from the cache"))))

;;; ── Running it ────────────────────────────────────────────────────

(defparameter *two-accounts*
  "{\"data\":{\"uiapi\":{\"query\":{\"Account\":{\"edges\":[
      {\"node\":{\"Id\":\"001A\",\"Name\":{\"value\":\"Acme\"},
                 \"Owner\":{\"Name\":{\"value\":\"Ada\"}},
                 \"Contacts\":{\"edges\":[{\"node\":{\"LastName\":{\"value\":\"Hopper\"}}}]}}},
      {\"node\":{\"Id\":\"001B\",\"Name\":{\"value\":null}}}],
    \"pageInfo\":{\"hasNextPage\":false,\"endCursor\":\"c\"}}}}}}")

(define-test gql/returns-a-hash-table-of-vectors-of-flattened-records
  :parent graphql
  (with-gql-org (requests :responses (list *two-accounts*))
    (let* ((result (cl-sf/objects:gql (Account (edges (node Id (Name value) (Owner (Name value))
                                                         (Contacts (edges (node (LastName value)))))))))
           (accounts (gethash "Account" result))
           (first (aref accounts 0)))
      (true (hash-table-p result))
      (is eq 'equal (hash-table-test result))
      (is = 1 (hash-table-count result))
      (true (simple-vector-p accounts))
      (is = 2 (length accounts))
      (is string= "001A" (cl-sf/objects:sobject-id first))
      (is string= "Account" (cl-sf/objects:sobject-type first))
      (let ((raw (cl-sf/objects:sobject-raw first)))
        (is string= "Acme" (gethash "Name" raw) "{ value } unwrapped")
        (is string= "Ada" (gethash "Name" (gethash "Owner" raw)) "parent record flattened")
        (let ((contacts (gethash "Contacts" raw)))
          (is = 1 (gethash "totalSize" contacts) "child connection as {totalSize, done, records}")
          (is string= "Hopper" (gethash "LastName" (aref (gethash "records" contacts) 0)))))
      (false (gethash "Name" (cl-sf/objects:sobject-raw (aref accounts 1))) "JSON null → NIL"))))

(define-test gql/records-are-instances-of-the-generated-class
  :parent graphql
  (with-gql-org (requests :responses
                          (list "{\"data\":{\"uiapi\":{\"query\":{\"GqlThing__c\":{\"edges\":[{\"node\":{\"Id\":\"a01\",\"Name\":{\"value\":\"T\"},\"Score__c\":{\"value\":7}}}]}}}}}"))
    (let* ((result (cl-sf/objects:gql (GqlThing__c (edges (node Id (Name value) (Score__c value))))))
           (thing  (aref (gethash "GqlThing__c" result) 0))
           (class  (gethash "GqlThing__c" cl-sf/objects:*sobject-classes*)))
      (true class)
      (true (typep thing class))
      (let ((hash (cl-sf/objects:sobject->hash-table thing)))
        (is string= "T" (gethash "Name" hash))
        (is = 7 (gethash "Score__c" hash))))))

(define-test gql/several-roots-and-aliases-key-the-result
  :parent graphql
  (with-gql-org (requests
                 :responses (list "{\"data\":{\"uiapi\":{\"query\":{\"Account\":{\"edges\":[{\"node\":{\"Id\":\"001A\"}}]},\"people\":{\"edges\":[{\"node\":{\"Id\":\"003A\"}},{\"node\":{\"Id\":\"003B\"}}]}}}}}"))
    (let ((result (cl-sf/objects:gql (Account (edges (node Id)))
                                     (Contact :as "people" (edges (node Id))))))
      (is = 2 (hash-table-count result))
      (is = 1 (length (gethash "Account" result)))
      (is = 2 (length (gethash "people" result)))
      (is string= "Contact" (cl-sf/objects:sobject-type (aref (gethash "people" result) 0)))
      (is = 1 (length requests) "one request for both"))))

(defun page (type ids &key more cursor)
  (format nil "{\"data\":{\"uiapi\":{\"query\":{\"~A\":{\"edges\":[~{{\"node\":{\"Id\":\"~A\"}}~^,~}],\"pageInfo\":{\"hasNextPage\":~A,\"endCursor\":\"~A\"}}}}}}"
          type ids (if more "true" "false") (or cursor "")))

(define-test gql/fetches-every-page
  :parent graphql
  (with-gql-org (requests :responses (list (page "Account" '("a" "b") :more t :cursor "c1")
                                           (page "Account" '("c" "d") :more t :cursor "c2")
                                           (page "Account" '("e") :more nil)))
    (let ((accounts (gethash "Account" (cl-sf/objects:gql (Account (edges (node Id)))))))
      (is equal '("a" "b" "c" "d" "e") (map 'list #'cl-sf/objects:sobject-id accounts))
      (is = 3 (length requests))
      (let ((texts (mapcar #'query-of (reverse requests))))
        (false (search "after:" (first texts)))
        (true (search "after: \"c1\"" (second texts)))
        (true (search "after: \"c2\"" (third texts)))))))

(define-test gql/an-explicit-first-is-one-page-even-if-more-exist
  :parent graphql
  (with-gql-org (requests :responses (list (page "Account" '("a" "b") :more t :cursor "c1")))
    (let ((accounts (gethash "Account" (cl-sf/objects:gql (Account :first 2 (edges (node Id)))))))
      (is = 2 (length accounts))
      (is = 1 (length requests)))))

(define-test gql/only-the-roots-with-more-pages-are-asked-again
  :parent graphql
  (with-gql-org (requests
                 :responses (list "{\"data\":{\"uiapi\":{\"query\":{\"Account\":{\"edges\":[{\"node\":{\"Id\":\"a1\"}}],\"pageInfo\":{\"hasNextPage\":true,\"endCursor\":\"x\"}},\"Contact\":{\"edges\":[{\"node\":{\"Id\":\"c1\"}}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":\"y\"}}}}}}"
                                  (page "Account" '("a2") :more nil)))
    (let ((result (cl-sf/objects:gql (Account (edges (node Id))) (Contact (edges (node Id))))))
      (is = 2 (length (gethash "Account" result)))
      (is = 1 (length (gethash "Contact" result)))
      (let ((second (query-of (first requests))))
        (true (search "Account(" second))
        (false (search "Contact(" second))))))

(define-test gql/an-initial-after-cursor-is-sent
  :parent graphql
  (is string=
      (wrap (format nil "Account(after: \"abc\", first: 2000, upperBound: 100000) { edges { node { Id } } ~A" *tail*))
      (gql-text (Account :after "abc" (edges (node Id))))))

(define-test gql/graphql-errors-signal
  :parent graphql
  (with-gql-org (requests :responses
                          (list "{\"data\":{\"uiapi\":{\"query\":{\"Account\":{\"edges\":[]}}}},\"errors\":[{\"message\":\"Field x is bad\",\"path\":[\"uiapi\",\"query\",\"Account\"]}]}"))
    (let ((condition (handler-case (progn (cl-sf/objects:gql (Account (edges (node Id)))) nil)
                       (cl-sf/api:sf-api-error (e) e))))
      (true condition)
      (true (search "Field x is bad" (cl-sf/api:sf-api-error-message condition)))
      (true (search "uiapi.query.Account" (cl-sf/api:sf-api-error-message condition))))))

(define-test gql/needs-a-connection
  :parent graphql
  (let ((cl-sf/auth:*connection* nil))
    (fail (cl-sf/objects:gql (Account (edges (node Id)))) 'cl-sf/auth:sf-auth-error)))

;;; ── Verified against a live org: dates, upperBound, error text ─────

(define-test gql/date-and-datetime-operands-are-objects
  :parent graphql
  (is string= "{CreatedDate: {lt: {value: \"2100-01-01T00:00:00Z\"}}}"
      (where-text (CreatedDate (lt "2100-01-01T00:00:00Z"))))
  (is string= "{CreatedDate: {lte: {literal: TODAY}}}" (where-text (CreatedDate (lte :today))))
  (is string= "{CreatedDate: {gte: {literal: LAST_WEEK}}}" (where-text (CreatedDate (gte :last-week))))
  (is string= "{CreatedDate: {gte: {range: {last_n_days: 7}}}}"
      (where-text (CreatedDate (gte (:range :last-n-days 7)))))
  (is string= "{CreatedDate: {gte: {range: {n_days_ago: 30}}}}"
      (where-text (CreatedDate (gte (:range :n-days-ago 30)))))
  ;; a ready-made object is left alone
  (is string= "{CreatedDate: {lt: {value: \"x\"}}}"
      (where-text (CreatedDate (lt (:raw "{value: \"x\"}")))))
  ;; a non-date field keeps its plain value
  (is string= "{Name: {lt: \"2100\"}}" (where-text (Name (lt "2100")))))

(define-test gql/timestamps-render-as-date-or-datetime-by-field-type
  :parent graphql
  (let ((ts (local-time:encode-timestamp 0 5 4 3 5 3 2024 :timezone local-time:+utc-zone+)))
    (is string= "{CreatedDate: {gt: {value: \"2024-03-05T03:04:05.000Z\"}}}"
        (where-text (CreatedDate (gt ts)))))
  (let ((ts (local-time:encode-timestamp 0 5 4 3 5 3 2024 :timezone local-time:+utc-zone+)))
    (is string= "Opportunity(where: {CloseDate: {gt: {value: \"2024-03-05\"}}}, first: 2000, upperBound: 100000) { edges { node { Id } } pageInfo { hasNextPage endCursor } }"
        (subseq (gql-text (Opportunity :where (CloseDate (gt ts)) (edges (node Id))))
                (length "query { uiapi { query { ")
                (- (length (gql-text (Opportunity :where (CloseDate (gt ts)) (edges (node Id)))))
                   (length " } } }"))))))

(define-test gql/a-full-fetch-lifts-the-2000-record-cap
  :parent graphql
  (is string=
      (wrap (format nil "Account(first: 2000, upperBound: 100000) { edges { node { Id } } ~A" *tail*))
      (gql-text (Account (edges (node Id)))))
  ;; your own :upper-bound replaces the default
  (is string=
      (wrap (format nil "Account(upperBound: 50, first: 2000) { edges { node { Id } } ~A" *tail*))
      (gql-text (Account :upper-bound 50 (edges (node Id)))))
  ;; an explicit :first is a single page: nothing is added
  (is string=
      (wrap (format nil "Account(first: 10) { edges { node { Id } } ~A" *tail*))
      (gql-text (Account :first 10 (edges (node Id))))))

(define-test gql/enum-keywords-use-underscores
  :parent graphql
  (is string=
      (wrap (format nil "Account(scope: ASSIGNED_TO_ME, first: 10) { edges { node { Id } } ~A" *tail*))
      (gql-text (Account :scope :assigned-to-me :first 10 (edges (node Id))))))

(define-test gql/error-messages-are-unescaped
  :parent graphql
  (with-gql-org (requests :responses
                          (list "{\"data\":{},\"errors\":[{\"message\":\"Field &#39;X__c&#39; in type &#39;Account&#39; is undefined &amp; &quot;bad&quot; &lt;b&gt;\"}]}"))
    (let ((condition (handler-case (progn (cl-sf/objects:gql (Account (edges (node Id)))) nil)
                       (cl-sf/api:sf-api-error (e) e))))
      (true (search "Field 'X__c' in type 'Account' is undefined & \"bad\" <b>"
                    (cl-sf/api:sf-api-error-message condition))))))

;;; ── LOCAL-TIME timestamps in SELECT conditions ────────────────────

(define-test temporal
  :parent cl-sf/tests
  :description "LOCAL-TIME values for date/datetime/time fields in select and gql.")

(defparameter *ts*
  (local-time:encode-timestamp 0 5 4 3 5 3 2024 :timezone local-time:+utc-zone+)
  "2024-03-05 03:04:05 UTC")

(defparameter *ts2*
  (local-time:encode-timestamp 0 0 0 0 6 3 2024 :timezone local-time:+utc-zone+))

(define-test temporal/select-renders-by-the-fields-type
  :parent temporal
  (with-gql-org (requests)
    (let ((ts *ts*))
      ;; datetime field → datetime literal
      (is string= "SELECT ID FROM ACCOUNT WHERE CREATEDDATE > 2024-03-05T03:04:05.000Z"
          (cl-sf/objects:soql select id from account where createddate > ts))
      ;; date field → date literal
      (is string= "SELECT ID FROM OPPORTUNITY WHERE CLOSEDATE <= 2024-03-05"
          (cl-sf/objects:soql select id from opportunity where closedate <= ts))
      ;; a Lisp expression, not just a variable
      (is string= "SELECT ID FROM OPPORTUNITY WHERE CLOSEDATE = 2024-03-06"
          (cl-sf/objects:soql select id from opportunity where closedate = (identity *ts2*)))
      ;; through a relationship
      (is string= "SELECT ID FROM CONTACT WHERE ACCOUNT.CREATEDDATE >= 2024-03-05T03:04:05.000Z"
          (cl-sf/objects:soql select id from contact where account.createddate >= ts)))))

(define-test temporal/select-renders-lists-of-timestamps
  :parent temporal
  (with-gql-org (requests)
    (let ((days (list *ts* *ts2*)))
      (is string= "SELECT ID FROM OPPORTUNITY WHERE CLOSEDATE IN (2024-03-05, 2024-03-06)"
          (cl-sf/objects:soql select id from opportunity where closedate in days)))))

(define-test temporal/select-falls-back-to-a-datetime-when-the-type-is-unknown
  :parent temporal
  (let ((ts *ts*) (cl-sf/auth:*connection* nil))
    ;; no connection at all (soql just builds text)
    (is string= "SELECT ID FROM ACCOUNT WHERE CREATEDDATE > 2024-03-05T03:04:05.000Z"
        (cl-sf/objects:soql select id from account where createddate > ts)))
  (with-gql-org (requests)
    (let ((ts *ts*))
      ;; a field the org doesn't know, an object it doesn't know
      (is string= "SELECT ID FROM ACCOUNT WHERE NOSUCH__C > 2024-03-05T03:04:05.000Z"
          (cl-sf/objects:soql select id from account where nosuch__c > ts))
      (is string= "SELECT ID FROM NOSUCHOBJECT__C WHERE X > 2024-03-05T03:04:05.000Z"
          (cl-sf/objects:soql select id from nosuchobject__c where x > ts)))))

(define-test temporal/select-only-looks-up-metadata-for-timestamps
  :parent temporal
  (let ((describes 0))
    (with-gql-org (requests :describes describes)
      (let ((name "Acme") (n 3) (ts *ts*))
        (cl-sf/objects:soql select id from account where name = name and annualrevenue > n)
        (is = 0 describes "strings and numbers never touch the schema")
        (cl-sf/objects:soql select id from account where createddate > ts)
        (cl-sf/objects:soql select id from account where createddate < ts)
        (is = 1 describes "one describe, then cached")))))

(define-test temporal/select-subquery-conditions-use-their-own-from
  :parent temporal
  (with-gql-org (requests)
    (let ((ts *ts*))
      (is string= "SELECT ID FROM CONTACT WHERE ACCOUNTID IN (SELECT ID FROM OPPORTUNITY WHERE CLOSEDATE < 2024-03-05)"
          (cl-sf/objects:soql select id from contact
                              where accountid in (select id from opportunity where closedate < ts))))))

(define-test temporal/timestamp-text
  :parent temporal
  (is string= "2024-03-05" (cl-sf/objects::%timestamp-text *ts* "date"))
  (is string= "2024-03-05T03:04:05.000Z" (cl-sf/objects::%timestamp-text *ts* "datetime"))
  (is string= "03:04:05.000Z" (cl-sf/objects::%timestamp-text *ts* "time"))
  ;; always UTC: 12:04:05 at UTC+9 is the same instant as 03:04:05 UTC
  (let ((tokyo (local-time:encode-timestamp 0 5 4 12 5 3 2024 :offset 32400)))
    (is string= "2024-03-05T03:04:05.000Z" (cl-sf/objects::%timestamp-text tokyo "datetime"))))

(define-test temporal/gql-uses-the-same-literals
  :parent temporal
  (let ((ts *ts*))
    (is string= "{CreatedDate: {gt: {value: \"2024-03-05T03:04:05.000Z\"}}}"
        (where-text (CreatedDate (gt ts))))))

;;; ── Dates and datetimes on returned objects are LOCAL-TIME timestamps ──

(defun ts (&rest args)
  "A UTC timestamp: (ts year month day [hour minute second [nsec]])."
  (destructuring-bind (year month day &optional (hour 0) (minute 0) (second 0) (nsec 0)) args
    (local-time:encode-timestamp nsec second minute hour day month year :offset 0)))

(define-test temporal/parses-what-salesforce-sends
  :parent temporal
  (flet ((parse (string &optional (kind "datetime")) (cl-sf/objects::%parse-temporal string kind)))
    ;; REST: +0000, GraphQL: Z, fractions
    (is local-time:timestamp= (ts 2024 3 5 3 4 5) (parse "2024-03-05T03:04:05.000+0000"))
    (is local-time:timestamp= (ts 2024 3 5 3 4 5) (parse "2024-03-05T03:04:05.000Z"))
    (is local-time:timestamp= (ts 2024 3 5 3 4 5) (parse "2024-03-05T03:04:05Z"))
    (is local-time:timestamp= (ts 2024 3 5 3 4 5 123000000) (parse "2024-03-05T03:04:05.123Z"))
    (is local-time:timestamp= (ts 2024 3 5 3 4 5 123456789) (parse "2024-03-05T03:04:05.123456789Z"))
    ;; offsets are applied: 12:04:05 at UTC+9 is 03:04:05 UTC
    (is local-time:timestamp= (ts 2024 3 5 3 4 5) (parse "2024-03-05T12:04:05+09:00"))
    (is local-time:timestamp= (ts 2024 3 5 3 4 5) (parse "2024-03-05T12:04:05+0900"))
    (is local-time:timestamp= (ts 2024 3 5 8 34 5) (parse "2024-03-05T03:04:05-05:30"))
    ;; dates are midnight UTC
    (is local-time:timestamp= (ts 2024 3 5) (parse "2024-03-05" "date"))
    (is local-time:timestamp= (ts 2024 3 5) (parse "2024-03-05"))
    (is local-time:timestamp= (ts 2024 3 5) (parse "2024-03-05T03:04:05.000Z" "date")
        "a date field ignores a time part")
    ;; things that aren't dates are not parsed
    (false (parse "hello"))
    (false (parse ""))
    (false (parse "2024-13-05"))
    (false (parse "2024-03-05T25:00:00Z"))
    (false (parse "2024-03-05T03:04:05.Z"))
    (false (parse "2024-03-05T03:04:05Zjunk"))
    (false (parse "03:04:05.000Z"))))

(define-test temporal/text-and-parse-round-trip
  :parent temporal
  (dolist (string '("2024-03-05T03:04:05.000Z" "2024-03-05T03:04:05.123Z" "1999-12-31T23:59:59.999Z"))
    (is string= string
        (cl-sf/objects::%timestamp-text (cl-sf/objects::%parse-temporal string "datetime") "datetime")))
  (is string= "2024-03-05"
      (cl-sf/objects::%timestamp-text (cl-sf/objects::%parse-temporal "2024-03-05" "date") "date")))

(cl-sf/objects:define-sobject-class temporal-probe "TemporalProbe__c"
  (name    "Name")
  (seen    "Seen__c"        :sf-type "datetime")
  (born    "Born__c"        :sf-type "date")
  (created "CreatedDate"    :sf-type "datetime"))

(defun probe-hash (&rest pairs)
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on pairs by #'cddr do (setf (gethash k h) v))
    h))

(define-test temporal/records-hold-timestamps-in-date-and-datetime-slots
  :parent temporal
  (let ((probe (cl-sf/objects:hash->sobject
                (probe-hash "Name" "n" "Seen__c" "2024-03-05T03:04:05.000+0000"
                            "Born__c" "1990-02-03" "CreatedDate" "2020-01-02T00:00:00.000Z")
                "TemporalProbe__c")))
    (is string= "n" (slot-value probe 'name))
    (is local-time:timestamp= (ts 2024 3 5 3 4 5) (slot-value probe 'seen))
    (is local-time:timestamp= (ts 1990 2 3) (slot-value probe 'born))
    (is local-time:timestamp= (ts 2020 1 2) (slot-value probe 'created))
    ;; the raw JSON is untouched
    (is string= "2024-03-05T03:04:05.000+0000"
        (gethash "Seen__c" (cl-sf/objects:sobject-raw probe)))))

(define-test temporal/unparseable-text-is-kept-and-null-is-empty
  :parent temporal
  (let ((probe (cl-sf/objects:hash->sobject
                (probe-hash "Name" 'null "Seen__c" "not a date" "Born__c" 'null)
                "TemporalProbe__c")))
    (is string= "not a date" (slot-value probe 'seen))
    (false (slot-value probe 'name) "a JSON null is an empty slot, not the symbol NULL")
    (false (slot-value probe 'born))))

(define-test temporal/timestamps-are-written-back-as-salesforce-text
  :parent temporal
  (let* ((probe (make-instance 'temporal-probe :name "n"
                                               :seen (ts 2024 3 5 3 4 5 123000000)
                                               :born (ts 1990 2 3)
                                               :created (ts 2020 1 2)))
         (hash  (cl-sf/objects:sobject->hash-table probe)))
    (is string= "2024-03-05T03:04:05.123Z" (gethash "Seen__c" hash))
    (is string= "1990-02-03" (gethash "Born__c" hash))
    (false (nth-value 1 (gethash "CreatedDate" hash)) "read-only: never sent"))
  ;; text a caller put in a slot themselves goes out untouched
  (let ((hash (cl-sf/objects:sobject->hash-table
               (make-instance 'temporal-probe :seen "2024-03-05T00:00:00Z"))))
    (is string= "2024-03-05T00:00:00Z" (gethash "Seen__c" hash))))

(define-test temporal/csv-writes-dates-and-datetimes-by-slot-kind
  :parent temporal
  (let ((probe (make-instance 'temporal-probe :name "n"
                                              :seen (ts 2024 3 5 3 4 5)
                                              :born (ts 1990 2 3))))
    (is string= (format nil "Name,Seen__c,Born__c~%n,2024-03-05T03:04:05.000Z,1990-02-03~%")
        (csv-text (cl-sf/bulk::%sobjects->csv (list probe) '("Name" "Seen__c" "Born__c"))))))

(define-test temporal/redefining-a-class-without-temporal-fields-forgets-them
  :parent temporal
  (cl-sf/objects:define-sobject-class kinds-probe "KindsProbe__c"
    (seen "Seen__c" :sf-type "datetime"))
  (true (gethash 'kinds-probe cl-sf/objects::*sobject-field-kinds*))
  (cl-sf/objects:define-sobject-class kinds-probe "KindsProbe__c"
    (seen "Seen__c"))
  (false (gethash 'kinds-probe cl-sf/objects::*sobject-field-kinds*)))

(define-test temporal/generated-classes-get-timestamp-slots-including-read-only-dates
  :parent temporal
  (with-gql-org (requests)
    (let* ((desc (find "GqlThing__c" *gql-org* :key (lambda (d) (gethash "name" d)) :test #'string=))
           (class-name (cl-sf/objects::%eval-define-sobject-class "GqlThing__c" desc))
           (slots (gethash class-name cl-sf/objects:*sobject-slot-maps*))
           (kinds (gethash class-name cl-sf/objects::*sobject-field-kinds*)))
      (true (assoc "Seen__c" slots :test #'equal))
      (true (assoc "CreatedDate" slots :test #'equal) "a read-only datetime is readable")
      (false (assoc "CreatedById" slots :test #'equal) "read-only non-dates stay out")
      (false (assoc "Id" slots :test #'equal))
      (is string= "datetime" (gethash "CreatedDate" kinds))
      (is string= "date" (gethash "Born__c" kinds))
      (false (gethash "Name" kinds)))))

(define-test temporal/gql-records-have-timestamps
  :parent temporal
  (with-gql-org (requests :responses
                          (list "{\"data\":{\"uiapi\":{\"query\":{\"GqlThing__c\":{\"edges\":[{\"node\":{\"Id\":\"a01\",\"Name\":{\"value\":\"T\"},\"Seen__c\":{\"value\":\"2024-03-05T03:04:05.000Z\"},\"Born__c\":{\"value\":\"1990-02-03\"},\"CreatedDate\":{\"value\":\"2020-01-02T00:00:00.000Z\"}}}]}}}}}"))
    (let* ((thing (aref (gethash "GqlThing__c"
                                 (cl-sf/objects:gql (GqlThing__c (edges (node Id (Name value) (Seen__c value)
                                                                         (Born__c value) (CreatedDate value))))))
                        0))
           (slots (gethash (class-name (class-of thing)) cl-sf/objects:*sobject-slot-maps*)))
      (flet ((slot (field) (slot-value thing (cdr (assoc field slots :test #'equal)))))
        (is local-time:timestamp= (ts 2024 3 5 3 4 5) (slot "Seen__c"))
        (is local-time:timestamp= (ts 1990 2 3) (slot "Born__c"))
        (is local-time:timestamp= (ts 2020 1 2) (slot "CreatedDate"))
        (is string= "T" (slot "Name"))))))

(define-test temporal/select-style-records-have-timestamps
  :parent temporal
  ;; the REST /query shape: JSON strings with +0000, parsed by HASH->SOBJECT
  (with-gql-org (requests)
    (let* ((desc (find "GqlThing__c" *gql-org* :key (lambda (d) (gethash "name" d)) :test #'string=)))
      (cl-sf/objects::%eval-define-sobject-class "GqlThing__c" desc)
      (let* ((attrs (probe-hash "type" "GqlThing__c"))
             (record (cl-sf/objects:hash->sobject
                      (probe-hash "attributes" attrs "Id" "a01" "Seen__c" "2024-03-05T03:04:05.000+0000"
                                  "CreatedDate" "2020-01-02T00:00:00.000+0000" "Born__c" 'null))))
        (let ((slots (gethash (class-name (class-of record)) cl-sf/objects:*sobject-slot-maps*)))
          (flet ((slot (field) (slot-value record (cdr (assoc field slots :test #'equal)))))
            (is local-time:timestamp= (ts 2024 3 5 3 4 5) (slot "Seen__c"))
            (is local-time:timestamp= (ts 2020 1 2) (slot "CreatedDate"))
            (false (slot "Born__c"))))))))

(define-test temporal/fast-timestamp-text-matches-local-times-formatter
  :parent temporal
  (let ((state (sb-ext:seed-random-state 7)) (mismatches '()))
    (flet ((check (timestamp)
             (dolist (kind '("date" "datetime" "time"))
               (let ((fast (cl-sf/objects::%timestamp-text timestamp kind))
                     (slow (cl-sf/objects::%timestamp-text-slow timestamp kind)))
                 (unless (string= fast slow) (push (list kind fast slow) mismatches))))))
      (dotimes (i 3000)                     ; instants from 1850 to 2400
        (check (local-time:make-timestamp :day (+ -54000 (random 220000 state))
                                          :sec (random 86400 state)
                                          :nsec (random 1000000000 state))))
      (loop for (year month day) in '((2000 2 29) (2000 3 1) (1999 12 31) (2100 2 28) (2100 3 1)
                                      (1900 2 28) (1900 3 1) (2024 2 29) (1970 1 1) (1 1 1) (9999 12 31))
            do (check (local-time:encode-timestamp 999999999 59 59 23 day month year :offset 0))
               (check (local-time:encode-timestamp 0 0 0 0 day month year :offset 0))))
    (is equal '() mismatches)))

;;; ── A field called Type must not collide with the sObject's own type ──

(define-test sobject/a-field-called-type-is-not-filled-with-the-object-name
  :parent temporal
  (with-gql-org (requests)
    (let* ((desc (find "GqlThing__c" *gql-org* :key (lambda (d) (gethash "name" d)) :test #'string=))
           (class-name (cl-sf/objects::%eval-define-sobject-class "GqlThing__c" desc))
           (slots (gethash class-name cl-sf/objects:*sobject-slot-maps*))
           (type-slot (cdr (assoc "Type" slots :test #'equal)))
           (thing (make-instance class-name)))
      (true type-slot "the Type field has a slot")
      (is string= "GqlThing__c" (cl-sf/objects:sobject-type thing) "the sObject type is still set")
      (false (slot-value thing type-slot) "...and the Type field is empty, not \"GqlThing__c\"")
      (false (nth-value 1 (gethash "Type" (cl-sf/objects:sobject->hash-table thing)))
             "an empty Type is not written back")
      ;; the field has its own initarg
      (let ((typed (make-instance class-name :type-field "Customer")))
        (is string= "Customer" (slot-value typed type-slot))
        (is string= "GqlThing__c" (cl-sf/objects:sobject-type typed))
        (is string= "Customer" (gethash "Type" (cl-sf/objects:sobject->hash-table typed))))
      ;; a fetched record whose Type is null keeps it empty
      (let ((fetched (cl-sf/objects:hash->sobject
                      (probe-hash "Id" "a01" "Name" "n" "Type" 'null) "GqlThing__c")))
        (false (slot-value fetched type-slot))
        (false (nth-value 1 (gethash "Type" (cl-sf/objects:sobject->hash-table fetched))))))))

(define-test sobject/define-sobject-class-renames-initargs-that-collide
  :parent temporal
  (cl-sf/objects:define-sobject-class collide-probe "CollideProbe__c"
    (type "Type")
    (name "Name"))
  (let ((o (make-instance 'collide-probe :name "n")))
    (is string= "CollideProbe__c" (cl-sf/objects:sobject-type o))
    (false (slot-value o 'type))
    (setf (slot-value o 'type) "x")
    (is string= "x" (collide-probe-type o))))

;;; ── Related records are CLOS objects ──────────────────────────────

(define-test related
  :parent cl-sf/tests
  :description "Parent records and child lists as CLOS objects, with a reader per relationship.")

(defun gql-class (name)
  "Generate (in the isolated registries of WITH-GQL-ORG) the class for the fixture object NAME."
  (cl-sf/objects::%eval-define-sobject-class
   name (find name *gql-org* :key (lambda (d) (gethash "name" d)) :test #'string=)))

(defun slot-by-field (object field)
  (slot-value object (cdr (assoc field (gethash (class-name (class-of object)) cl-sf/objects:*sobject-slot-maps*)
                                 :test #'equal))))

(defparameter *soql-shaped-thing*
  "{\"attributes\":{\"type\":\"GqlThing__c\"},\"Id\":\"a01\",\"Name\":\"T\",
    \"Owner\":{\"attributes\":{\"type\":\"User\"},\"Id\":\"005\",\"Name\":\"Ada\"},
    \"Notes__r\":{\"totalSize\":2,\"done\":true,\"records\":[
        {\"attributes\":{\"type\":\"GqlNote__c\"},\"Name\":\"first\"},
        {\"attributes\":{\"type\":\"GqlNote__c\"},\"Name\":\"second\"}]},
    \"Parent\":null,
    \"BillingAddress\":{\"city\":\"Oslo\"}}")

(define-test related/parents-and-child-lists-are-clos-objects
  :parent related
  (with-gql-org (requests)
    (gql-class "User") (gql-class "GqlNote__c") (gql-class "GqlThing__c")
    (let* ((thing (cl-sf/objects:hash->sobject (json *soql-shaped-thing*)))
           (owner (cl-sf/objects:sobject-related thing "Owner"))
           (notes (cl-sf/objects:sobject-related thing "Notes__r")))
      ;; the parent: one typed object with its own fields as slots
      (true (typep owner (gethash "User" cl-sf/objects:*sobject-classes*)))
      (is string= "User" (cl-sf/objects:sobject-type owner))
      (is string= "005" (cl-sf/objects:sobject-id owner))
      (is string= "Ada" (slot-by-field owner "Name"))
      ;; the child list: a vector of typed objects
      (true (simple-vector-p notes))
      (is = 2 (length notes))
      (true (every (lambda (n) (typep n (gethash "GqlNote__c" cl-sf/objects:*sobject-classes*))) notes))
      (is equal '("first" "second") (map 'list (lambda (n) (slot-by-field n "Name")) notes))
      ;; null, absent and compound values are not relationships
      (false (cl-sf/objects:sobject-related thing "Parent"))
      (false (cl-sf/objects:sobject-related thing "Nowhere"))
      (false (cl-sf/objects:sobject-related thing "BillingAddress"))
      (is equal '("Owner" "Notes__r") (mapcar #'car (cl-sf/objects:sobject-related thing)))
      ;; lookup ignores case; the raw JSON is untouched
      (is eq owner (cl-sf/objects:sobject-related thing "owner"))
      (true (hash-table-p (gethash "Owner" (cl-sf/objects:sobject-raw thing)))))))

(define-test related/each-relationship-has-a-reader-named-after-it
  :parent related
  (with-gql-org (requests)
    (gql-class "User") (gql-class "GqlNote__c")
    (let* ((class-name (gql-class "GqlThing__c"))
           (thing (cl-sf/objects:hash->sobject (json *soql-shaped-thing*)))
           (owner-reader (find-symbol "GQLTHING__C-OWNER" (symbol-package class-name)))
           (notes-reader (find-symbol "GQLTHING__C-NOTES__R" (symbol-package class-name))))
      (true (and owner-reader (fboundp owner-reader)))
      (is string= "Ada" (slot-by-field (funcall owner-reader thing) "Name"))
      (is = 2 (length (funcall notes-reader thing)))
      ;; and a reader never replaces a field's own accessor
      (let ((name-accessor (find-symbol "GQLTHING__C-NAME" (symbol-package class-name))))
        (is string= "T" (funcall name-accessor thing))))))

(define-test related/a-nested-record-of-an-unknown-class-is-a-generic-object
  :parent related
  (with-gql-org (requests)
    (gql-class "GqlThing__c")                ; the User and GqlNote__c classes are not registered
    (let* ((thing (cl-sf/objects:hash->sobject (json *soql-shaped-thing*)))
           (owner (cl-sf/objects:sobject-related thing "Owner")))
      (is eq 'cl-sf/objects:sobject (class-name (class-of owner)))
      (is string= "User" (cl-sf/objects:sobject-type owner))
      (is string= "Ada" (gethash "Name" (cl-sf/objects:sobject-raw owner))))))

(define-test related/nested-records-nest
  :parent related
  (with-gql-org (requests)
    (let* ((record (json "{\"attributes\":{\"type\":\"Contact\"},\"LastName\":\"G\",
                           \"Account\":{\"attributes\":{\"type\":\"Account\"},\"Name\":\"Acme\",
                                        \"Owner\":{\"attributes\":{\"type\":\"User\"},\"Name\":\"Ada\"}}}"))
           (contact (cl-sf/objects:hash->sobject record))
           (account (cl-sf/objects:sobject-related contact "Account"))
           (owner   (cl-sf/objects:sobject-related account "Owner")))
      (is string= "Acme" (gethash "Name" (cl-sf/objects:sobject-raw account)))
      (is string= "Ada" (gethash "Name" (cl-sf/objects:sobject-raw owner))))))

(define-test related/printing-shows-the-related-objects
  :parent related
  (with-gql-org (requests)
    (gql-class "User") (gql-class "GqlNote__c") (gql-class "GqlThing__c")
    (let ((text (princ-to-string (cl-sf/objects:hash->sobject (json *soql-shaped-thing*)))))
      (true (search "Owner=User(" text))
      (true (search "Name=Ada" text))
      (true (search "Notes__r=[GqlNote__c(" text))
      (false (search "NULL" text) "a JSON null prints as NIL, not NULL")
      (true (search "Parent=NIL" text)))))

(define-test related/the-query-in-the-report-is-correct-a-user-with-no-contact-is-just-nil
  :parent related
  (with-gql-org (requests)
    (gql-class "GqlThing__c")
    (let ((user (cl-sf/objects:hash->sobject
                 (json "{\"attributes\":{\"type\":\"GqlThing__c\"},\"Name\":\"u\",\"Contact\":null}"))))
      (false (cl-sf/objects:sobject-related user "Contact"))
      (is equal "GqlThing__c(Name=u, Contact=NIL)" (princ-to-string user)))))

(define-test related/gql-parents-and-children-are-typed-from-metadata
  :parent related
  (with-gql-org (requests :responses
                          (list "{\"data\":{\"uiapi\":{\"query\":{\"GqlThing__c\":{\"edges\":[{\"node\":{\"Id\":\"a01\",\"Name\":{\"value\":\"T\"},\"Owner\":{\"Name\":{\"value\":\"Ada\"}},\"Notes__r\":{\"edges\":[{\"node\":{\"Name\":{\"value\":\"n1\"}}}]}}}]}}}}}"))
    (gql-class "User") (gql-class "GqlNote__c")
    (let* ((thing (aref (gethash "GqlThing__c"
                                 (cl-sf/objects:gql
                                   (GqlThing__c (edges (node Id (Name value) (Owner (Name value))
                                                         (Notes__r (edges (node (Name value)))))))))
                        0))
           (owner (cl-sf/objects:sobject-related thing "Owner"))
           (notes (cl-sf/objects:sobject-related thing "Notes__r")))
      (is string= "User" (cl-sf/objects:sobject-type owner))
      (true (typep owner (gethash "User" cl-sf/objects:*sobject-classes*)))
      (is string= "Ada" (slot-by-field owner "Name"))
      (is = 1 (length notes))
      (is string= "GqlNote__c" (cl-sf/objects:sobject-type (aref notes 0))))))

(define-test related/gql-polymorphic-parents-take-the-typename
  :parent related
  (with-gql-org (requests :responses
                          (list "{\"data\":{\"uiapi\":{\"query\":{\"Lead\":{\"edges\":[{\"node\":{\"Id\":\"00Q\",\"Owner\":{\"__typename\":\"User\",\"Name\":{\"value\":\"Ada\"}}}}]}}}}}"))
    (let* ((lead (aref (gethash "Lead"
                                (cl-sf/objects:gql (Lead (edges (node Id (Owner (:on User (Name value)) __typename))))))
                       0))
           (owner (cl-sf/objects:sobject-related lead "Owner")))
      (is string= "User" (cl-sf/objects:sobject-type owner)))))

;;; ── Bulk: the two bugs the live run found ─────────────────────────

(define-test bulk-column-names/upsert-on-id-includes-the-id-column
  :parent bulk
  ;; Id is never in a generated slot map, so upserting on Id sent no Id at all
  (let ((cols (cl-sf/bulk::%bulk-column-names (list (make-widget :label "X")) :upsert "Id")))
    (is string= "Id" (first cols))
    (true (member "Label__c" cols :test #'equal)))
  ;; an external id that is an ordinary field is still just one column
  (let ((cols (cl-sf/bulk::%bulk-column-names (list (make-widget :label "X")) :upsert "Label__c")))
    (is = 1 (count "Label__c" cols :test #'equal))))

(define-test sobjects->csv/upsert-on-id-writes-the-ids
  :parent bulk
  (let ((existing (make-widget :label "A"))
        (fresh    (make-widget :label "B")))
    (setf (cl-sf/objects:sobject-id existing) "001EXISTING")
    (let ((columns (cl-sf/bulk::%bulk-column-names (list existing fresh) :upsert "Id")))
      (is string= (format nil "Id,Label__c,Amount__c,Active__c~%001EXISTING,A,,~%,B,,~%")
          (csv-text (cl-sf/bulk::%sobjects->csv (list existing fresh) columns))))))

(define-test build-bulk-result/a-failed-row-with-a-blank-result-id-is-matched-by-the-sent-id
  :parent bulk
  (let ((ghost (make-instance 'cl-sf/objects:sobject :id "006000000000000AAA" :type "Opportunity"))
        (real  (make-instance 'cl-sf/objects:sobject :id "006REAL" :type "Opportunity")))
    (with-stubbed-results
        (:success (format nil "\"sf__Id\",\"sf__Created\",\"Id\"~%\"006REAL\",\"false\",\"006REAL\"~%")
         :failed (format nil "\"sf__Id\",\"sf__Error\",\"Id\",\"Name\"~%\"\",\"INVALID_CROSS_REFERENCE_KEY:invalid cross reference id:--\",\"006000000000000AAA\",\"ghost\"~%")
         :unprocessed "")
      (let ((result (cl-sf/bulk::%build-bulk-result nil "J" (list real ghost) :update "Opportunity"
                                                    (job-status "JobComplete" :processed 2 :failed 1))))
        (is eq real (first (cl-sf/bulk:bulk-result-successful result)))
        (is = 1 (length (cl-sf/bulk:bulk-result-failed result)))
        (is eq ghost (car (first (cl-sf/bulk:bulk-result-failed result))))
        (true (search "INVALID_CROSS_REFERENCE_KEY" (cdr (first (cl-sf/bulk:bulk-result-failed result)))))))))
