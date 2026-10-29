;;;; cl-sf/src/graphql.lisp
;;;; GQL — Salesforce's GraphQL API (POST /services/data/vNN.N/graphql),
;;;; written as s-expressions that mirror GraphQL's own structure.
;;;;
;;;;   (gql
;;;;     (Account :where (Industry (in "Tech" "Finance"))
;;;;              :order-by (Name :desc)
;;;;              :first 10
;;;;       (edges (node
;;;;         Id (Name value)
;;;;         (Owner (Name value) (Email value))
;;;;         (Contacts :first 5
;;;;           (edges (node (LastName value) (Email value))))))))
;;;;
;;;; GraphQL names are case-sensitive but the Lisp reader upcases symbols, so
;;;; object, field and relationship names are written as bare symbols and
;;;; their real spelling is recovered, case-insensitively, from the org's
;;;; describe metadata (fetched once per object type, then cached).  A STRING
;;;; anywhere a name goes is used exactly as written.
;;;;
;;;; The macro hides only the plumbing: uiapi { query { ... } }, and the
;;;; pageInfo/after paging of each top-level query.  It returns a hash table
;;;; (EQUAL) from each top-level name (or its :AS alias) to a vector of CLOS
;;;; records, every page fetched.

(in-package #:cl-sf/objects)

;;; ──────────────────────────────────────────────────────────────────
;;; Syntax → AST   (macro time; no network)
;;;
;;; Names stay symbols and values become forms, so the expansion is a
;;; builder that runs with the caller's variables in scope:
;;;   (:root object alias args selections)
;;;   selection  (:leaf name) | (:field name args selections) | (:on type selections)
;;;   arg        (name-string . value-ast), value-ast one of
;;;                (:value lisp-object) (:enum "X") (:raw "text")
;;;                (:filter f) (:order entries)
;;; ──────────────────────────────────────────────────────────────────

(defparameter *gql-operators*
  '("eq" "ne" "gt" "gte" "lt" "lte" "in" "nin" "like" "includes" "excludes"))

(defparameter *gql-array-operators* '("in" "nin" "includes" "excludes")
  "Operators whose argument is an array.")

(defun %gql-operator (x)
  "The GraphQL operator named by the symbol X (case-insensitively), or NIL."
  (and x (symbolp x) (find (symbol-name x) *gql-operators* :test #'string-equal)))

(defun %kebab->camel (name)
  "\"ORDER-BY\" → \"orderBy\"."
  (let ((parts (uiop:split-string (string-downcase name) :separator "-")))
    (apply #'concatenate 'string (first parts) (mapcar #'string-capitalize (rest parts)))))

(defun %gql-split-args (forms)
  "Leading :key value pairs of FORMS → (values alist rest)."
  (let ((args '()))
    (loop while (and (consp forms) (keywordp (car forms)))
          do (unless (cdr forms)
               (error "cl-sf: gql argument ~S has no value" (car forms)))
             (push (cons (pop forms) (pop forms)) args))
    (values (nreverse args) forms)))

(defun %gql-value-form (v env)
  "A form that builds the value-ast for V.  Strings and numbers are values;
   a keyword is an enum (:desc → DESC); true/false/null are literals; a
   symbol bound in the surrounding code is its value; (:raw \"text\") is
   verbatim GraphQL; any other list is a Lisp expression."
  (cond ((or (stringp v) (numberp v)) `(list :value ,v))
        ((null v)     '(list :value nil))
        ((eq v t)     '(list :value t))
        ((keywordp v) `(list :enum ,(substitute #\_ #\- (symbol-name v))))
        ((symbolp v)  (cond ((member (symbol-name v) '("TRUE" "FALSE" "NULL") :test #'string=)
                             `(list :raw ,(string-downcase (symbol-name v))))
                            ((%soql-variable-p v env) `(list :value ,v))
                            (t `(list :enum ,(symbol-name v)))))
        ((eq (car v) :raw) `(list :raw ,(second v)))
        ((eq (car v) :range)
         `(list :range ,(substitute #\_ #\- (string-downcase (symbol-name (second v))))
                ,(%gql-value-form (third v) env)))
        ((consp v)    `(list :value ,v))
        (t (error "cl-sf: malformed gql value ~S" v))))

(defun %gql-filter-item-form (x env)
  (let ((op (and (consp x) (%gql-operator (car x)))))
    (if op
        `(list :op ,op ,@(mapcar (lambda (v) (%gql-value-form v env)) (cdr x)))
        (%gql-filter-form x env))))

(defun %gql-filter-form (f env)
  "(Industry (in \"A\" \"B\")) · (Account (Industry (eq \"x\"))) · (and f...) ·
   (or f...) · (not f)"
  (unless (consp f)
    (error "cl-sf: malformed gql filter ~S" f))
  (let ((head (car f)))
    (cond ((%soql-kw= head "AND")
           `(list :and ,@(mapcar (lambda (x) (%gql-filter-form x env)) (cdr f))))
          ((%soql-kw= head "OR")
           `(list :or ,@(mapcar (lambda (x) (%gql-filter-form x env)) (cdr f))))
          ((%soql-kw= head "NOT")
           `(list :not ,(%gql-filter-form (second f) env)))
          (t `(list :filter ',head ,@(mapcar (lambda (x) (%gql-filter-item-form x env)) (cdr f)))))))

(defun %gql-order-entry-form (entry)
  "(Name :desc) · (Name :desc :nulls :last) · (Name (order :desc) (nulls :last))"
  (destructuring-bind (field &rest spec) entry
    (let (order nulls)
      (loop while spec
            do (let ((x (pop spec)))
                 (cond ((member x '(:asc :desc)) (setf order (symbol-name x)))
                       ((eq x :nulls) (setf nulls (symbol-name (pop spec))))
                       ((and (consp x) (%soql-kw= (car x) "ORDER"))
                        (setf order (symbol-name (second x))))
                       ((and (consp x) (%soql-kw= (car x) "NULLS"))
                        (setf nulls (symbol-name (second x))))
                       (t (error "cl-sf: malformed gql order-by entry ~S" entry)))))
      `(list ',field ,order ,nulls))))

(defun %gql-order-form (v)
  `(list ,@(mapcar #'%gql-order-entry-form (if (consp (car v)) v (list v)))))

(defun %gql-arg-forms (args env)
  (loop for (key . value) in args
        unless (eq key :as)
          collect `(cons ,(%kebab->camel (symbol-name key))
                         ,(case key
                            (:where    `(list :filter ,(%gql-filter-form value env)))
                            (:order-by `(list :order ,(%gql-order-form value)))
                            (t         (%gql-value-form value env))))))

(defun %gql-selection-form (item env)
  (cond ((or (stringp item) (symbolp item))
         `(list :leaf ',item))
        ((eq (car item) :on)
         `(list :on ',(second item) (list ,@(mapcar (lambda (i) (%gql-selection-form i env))
                                                    (cddr item)))))
        ((consp item)
         (multiple-value-bind (args children) (%gql-split-args (cdr item))
           `(list :field ',(car item)
                  (list ,@(%gql-arg-forms args env))
                  (list ,@(mapcar (lambda (i) (%gql-selection-form i env)) children)))))
        (t (error "cl-sf: malformed gql selection ~S" item))))

(defun %gql-root-form (root env)
  (destructuring-bind (object &rest rest) root
    (multiple-value-bind (args selections) (%gql-split-args rest)
      (let ((alias (cdr (assoc :as args))))
        `(list :root ',object
               ,(cond ((null alias) nil)
                      ((stringp alias) alias)
                      (t (string-downcase (symbol-name alias))))
               (list ,@(%gql-arg-forms args env))
               (list ,@(mapcar (lambda (i) (%gql-selection-form i env)) selections)))))))

(defparameter *gql-structural-names*
  '("edges" "node" "cursor" "pageInfo" "hasNextPage" "hasPreviousPage" "startCursor"
    "endCursor" "totalCount" "value" "displayValue" "label" "__typename")
  "GraphQL's own (not Salesforce's) field names, with their real spelling.")

(defun %gql-structural (head)
  "The GraphQL spelling of the structural word HEAD (edges, node, display-value, …)."
  (if (stringp head)
      head
      (let ((u (remove #\- (string-upcase (symbol-name head)))))
        (or (find u *gql-structural-names* :key #'string-upcase :test #'string=)
            (%kebab->camel (symbol-name head))))))

;;; ──────────────────────────────────────────────────────────────────
;;; AST → GraphQL text
;;; ──────────────────────────────────────────────────────────────────

(defparameter *gql-page-size* 2000
  "Records per request when a top-level query gives no :first (Salesforce's
   maximum: 2000, confirmed against a live org).")

(defparameter *gql-upper-bound* 100000
  "Without an upperBound, Salesforce stops paging at 2000 records in total.  A
   query that fetches everything (no :first) therefore sends this, unless it
   names its own :upper-bound.  ponytail: 100,000 is the ceiling I know of; the
   API accepted much larger values on small results, so I couldn't confirm
   where it really is — lower it if Salesforce rejects it.")

(defun %gql-render-lisp (x)
  "A Lisp value as a GraphQL literal."
  (etypecase x
    (string  (com.inuoe.jzon:stringify x))
    (null    "null")
    ((eql t) "true")
    (keyword (symbol-name x))
    (integer (princ-to-string x))
    (real    (%soql-render-value x))
    (local-time:timestamp (com.inuoe.jzon:stringify (%timestamp-text x "datetime")))
    (hash-table (format nil "{~{~A~^, ~}}"
                        (loop for k being the hash-keys of x using (hash-value v)
                              collect (format nil "~A: ~A" k (%gql-render-lisp v)))))
    (sequence (format nil "[~{~A~^, ~}]" (map 'list #'%gql-render-lisp x)))))

(defun %gql-value-text (ast)
  (ecase (car ast)
    (:value (%gql-render-lisp (second ast)))
    ((:enum :raw) (second ast))))

(defun %gql-array-text (asts)
  "ASTS as one GraphQL array; a single runtime sequence supplies the elements."
  (let* ((single (and (null (cdr asts)) (eq (car (first asts)) :value)))
         (value  (and single (second (first asts)))))
    (format nil "[~{~A~^, ~}]"
            (if (and single (typep value 'sequence) (not (stringp value)))
                (map 'list #'%gql-render-lisp value)      ; includes an empty list → []
                (mapcar #'%gql-value-text asts)))))

(defun %gql-date-text (ast field-type)
  "AST as the operand of a date or datetime field, which GraphQL takes as an
   object, not a bare value:  a string or timestamp → {value: ...};
   :today → {literal: TODAY}; (:range :last-n-days 7) → {range: {last_n_days: 7}};
   anything else (:raw, a ready-made object) is passed through."
  (case (car ast)
    (:enum  (format nil "{literal: ~A}" (second ast)))
    (:range (format nil "{range: {~A: ~A}}" (second ast) (%gql-value-text (third ast))))
    (:value (let ((x (second ast)))
              (typecase x
                (string (format nil "{value: ~A}" (%gql-render-lisp x)))
                (local-time:timestamp
                 (format nil "{value: ~A}"
                         (com.inuoe.jzon:stringify (%timestamp-text x field-type))))
                (t (%gql-value-text ast)))))
    (t (%gql-value-text ast))))

(defun %gql-op-text (item &optional field-type)
  (unless (eq (car item) :op)
    (%gql-error "a scalar field takes operators like (eq ...) or (in ...), not ~S" item))
  (destructuring-bind (op &rest values) (cdr item)
    (format nil "~A: ~A" op
            (cond ((member op *gql-array-operators* :test #'string=) (%gql-array-text values))
                  ((and values (null (cdr values)))
                   (if (member field-type '("date" "datetime") :test #'equal)
                       (%gql-date-text (first values) field-type)
                       (%gql-value-text (first values))))
                  (t (%gql-error "operator ~A takes exactly one value" op))))))

(defun %gql-filter-text (f type conn)
  (flet ((sub (x) (%gql-filter-text x type conn)))
    (ecase (car f)
      (:and (format nil "{and: [~{~A~^, ~}]}" (mapcar #'sub (cdr f))))
      (:or  (format nil "{or: [~{~A~^, ~}]}" (mapcar #'sub (cdr f))))
      (:not (format nil "{not: ~A}" (sub (second f))))
      (:filter
       (destructuring-bind (head &rest items) (cdr f)
         (multiple-value-bind (name kind data) (%gql-lookup type head)
           (format nil "{~A: ~A}" name
                   (ecase kind
                     ((:scalar :verbatim)
                      (let ((field-type (gethash (string-upcase name) (gqt-types type))))
                        (format nil "{~{~A~^, ~}}"
                                (mapcar (lambda (item) (%gql-op-text item field-type)) items))))
                     (:parent
                      (let* ((ptype (%gql-type conn (if (cdr data)
                                                         (%gql-error "~A is polymorphic; it cannot be filtered through" name)
                                                         (car data))))
                             (texts (mapcar (lambda (x) (%gql-filter-text x ptype conn)) items)))
                        (if (cdr texts)
                            (format nil "{and: [~{~A~^, ~}]}" texts)
                            (first texts))))
                     (:child (%gql-error "cannot filter on the child relationship ~A" name))))))))))

(defun %gql-order-text (entries type)
  (format nil "{~{~A~^, ~}}"
          (mapcar (lambda (entry)
                    (destructuring-bind (head order nulls) entry
                      (format nil "~A: {~{~A~^, ~}}" (values (%gql-lookup type head))
                              (append (and order (list (format nil "order: ~A" order)))
                                      (and nulls (list (format nil "nulls: ~A" nulls)))))))
                  entries)))

(defun %gql-args-text (args type conn)
  (if (null args)
      ""
      (format nil "(~{~A~^, ~})"
              (mapcar (lambda (arg)
                        (destructuring-bind (name . ast) arg
                          (format nil "~A: ~A" name
                                  (ecase (car ast)
                                    (:filter (%gql-filter-text (second ast) type conn))
                                    (:order  (%gql-order-text (second ast) type))
                                    ((:value :enum :raw) (%gql-value-text ast))))))
                      args))))

(declaim (ftype function %gql-braced))

(defun %gql-field-text (head args children kind type conn)
  (flet ((braced (kind type) (%gql-braced children kind type conn)))
    (ecase kind
      (:record
       (multiple-value-bind (name fkind data) (%gql-lookup type head)
         (ecase fkind
           ((:scalar :verbatim)
            (format nil "~A~A ~A" name (%gql-args-text args nil conn) (braced :field nil)))
           (:parent
            (when (null data)
              (%gql-error "~A refers to no sObject" name))
            (format nil "~A ~A" name
                    (if (cdr data)                    ; polymorphic: needs (:on Type ...)
                        (braced :poly nil)
                        (braced :record (%gql-type conn (car data))))))
           (:child
            (let ((ctype (%gql-type conn data)))
              (format nil "~A~A ~A" name (%gql-args-text args ctype conn)
                      (braced :connection ctype)))))))
      (:connection
       (let ((name (%gql-structural head)))
         (format nil "~A~A ~A" name (%gql-args-text args type conn)
                 (braced (if (equal name "edges") :edge :field) type))))
      (:edge
       (let ((name (%gql-structural head)))
         (format nil "~A ~A" name
                 (if (equal name "node") (braced :record type) (braced :field nil)))))
      (:field
       (format nil "~A ~A" (%gql-structural head) (braced :field nil)))
      (:poly
       (%gql-error "a polymorphic relationship needs (:on Type ...) for its fields, not ~A" (%gql-name head))))))

(defun %gql-item-text (item kind type conn)
  (ecase (car item)
    (:leaf (let ((head (second item)))
             (ecase kind
               (:record (values (%gql-lookup type head)))
               ((:connection :edge :field) (%gql-structural head))
               (:poly (let ((s (%gql-structural head)))
                        (unless (equal s "__typename")
                          (%gql-error "a polymorphic relationship needs (:on Type ...) for ~A" (%gql-name head)))
                        s)))))
    (:on (destructuring-bind (type-name children) (cdr item)
           (let ((target (%gql-type conn type-name)))
             (format nil "... on ~A ~A" (gqt-name target)
                     (%gql-braced children :record target conn)))))
    (:field (destructuring-bind (head args children) (cdr item)
              (%gql-field-text head args children kind type conn)))))

(defun %gql-braced (items kind type conn)
  (when (null items)
    (%gql-error "a selection with nothing selected"))
  (format nil "{ ~{~A~^ ~} }"
          (mapcar (lambda (item) (%gql-item-text item kind type conn)) items)))

;;; ──────────────────────────────────────────────────────────────────
;;; Running it
;;; ──────────────────────────────────────────────────────────────────

(defstruct (gql-state (:conc-name gqs-))
  root type key limited-p cursor (pages '()))

(defun %gql-root-text (state conn)
  (destructuring-bind (object alias args selections) (cdr (gqs-root state))
    (declare (ignore object))
    (let* ((type   (gqs-type state))
           (cursor (gqs-cursor state))
           (args   (append (if cursor
                               (remove "after" args :key #'car :test #'equal)
                               args)
                           (and cursor `(("after" :value ,cursor)))
                           (and (not (gqs-limited-p state))
                                `(("first" :value ,*gql-page-size*)))
                           (and (not (gqs-limited-p state))
                                (not (assoc "upperBound" args :test #'equal))
                                `(("upperBound" :value ,*gql-upper-bound*))))))
      (format nil "~@[~A: ~]~A~A { ~A pageInfo { hasNextPage endCursor } }"
              alias (gqt-name type) (%gql-args-text args type conn)
              (format nil "~{~A~^ ~}"
                      (mapcar (lambda (item) (%gql-item-text item :connection type conn))
                              selections))))))

(defun %gql-query-text (states conn)
  (format nil "query { uiapi { query { ~{~A~^ ~} } } }"
          (mapcar (lambda (state) (%gql-root-text state conn)) states)))

(defun %html-unescape (string)
  "Undo the HTML escaping Salesforce applies to GraphQL error messages."
  (let ((s string))
    (loop for (entity . char) in '(("&#39;" . "'") ("&quot;" . "\"") ("&lt;" . "<")
                                    ("&gt;" . ">") ("&amp;" . "&"))
          do (setf s (cl-ppcre:regex-replace-all (cl-ppcre:quote-meta-chars entity) s char)))
    s))

(defun %gql-errors-text (errors)
  (format nil "~{~A~^; ~}"
          (map 'list (lambda (e)
                       (let ((message (%html-unescape (princ-to-string (gethash "message" e))))
                             (path (gethash "path" e)))
                         (if (and path (plusp (length path)))
                             (format nil "~A (at ~{~A~^.~})" message (coerce path 'list))
                             message)))
               errors)))

(defun %gql-typed (record type-name)
  "RECORD (a flattened hash) marked with its sObject type, as a SOQL record
   would be, so HASH->SOBJECT can build the right class for it."
  (when (and type-name (not (nth-value 1 (gethash "attributes" record))))
    (let ((attributes (make-hash-table :test #'equal)))
      (setf (gethash "type" attributes) type-name
            (gethash "attributes" record) attributes)))
  record)

(defun %gql-flatten-value (key v type conn)
  (cond ((hash-table-p v)
         (cond ((nth-value 1 (gethash "edges" v))      ; a child connection
                (multiple-value-bind (name kind data) (if type (%schema-lookup type key) nil)
                  (declare (ignore name))
                  (let* ((child-type (and (eq kind :child) (%gql-type conn data)))
                         (records (map 'vector
                                       (lambda (edge)
                                         (%gql-typed (%gql-flatten (gethash "node" edge) child-type conn)
                                                     (and child-type (gqt-name child-type))))
                                       (gethash "edges" v)))
                         (wrapper (make-hash-table :test #'equal)))
                    (setf (gethash "totalSize" wrapper) (length records)
                          (gethash "done" wrapper) t
                          (gethash "records" wrapper) records)
                    wrapper)))
               ((and (= (hash-table-count v) 1) (nth-value 1 (gethash "value" v)))
                (%gql-flatten-value key (gethash "value" v) nil conn))   ; { value } → value
               (t                                       ; a parent record
                (multiple-value-bind (name kind data) (if type (%schema-lookup type key) nil)
                  (declare (ignore name))
                  (let ((parent-type (and (eq kind :parent) data (null (cdr data))
                                          (%gql-type conn (car data)))))
                    (%gql-typed (%gql-flatten v parent-type conn)
                                (cond (parent-type (gqt-name parent-type))
                                      ((stringp (gethash "__typename" v)) (gethash "__typename" v)))))))))
        ((eq v 'null) nil)
        (t v)))

(defun %gql-flatten (node type conn)
  "A GraphQL record as the REST-style hash the rest of cl-sf expects: field
   wrappers unwrapped, child connections turned into {totalSize, done, records},
   and related records marked with their sObject type (from TYPE's metadata,
   or the record's __typename).  TYPE is NODE's GQL-TYPE, if known."
  (let ((out (make-hash-table :test #'equal :size (max 7 (hash-table-count node)))))
    (maphash (lambda (k v) (setf (gethash k out) (%gql-flatten-value k v type conn))) node)
    out))

(defun %gql-run (roots)
  "Run the gql ROOTS (see the AST above) on *CONNECTION*; see GQL."
  (let ((conn *connection*))
    (unless conn
      (error 'sf-auth-error
             :message "No connection — wrap the call in WITH-CONN."))
    (let* ((states (mapcar (lambda (root)
                             (destructuring-bind (object alias args selections) (cdr root)
                               (declare (ignore selections))
                               (let ((type (%gql-type conn object :register-class t)))
                                 (make-gql-state
                                  :root root :type type
                                  :key (or alias (gqt-name type))
                                  :limited-p (and (assoc "first" args :test #'equal) t)
                                  :cursor (let ((after (cdr (assoc "after" args :test #'equal))))
                                            (and after (eq (car after) :value) (second after)))))))
                           roots))
           (pending states))
      (loop while pending
            do (let* ((body (let ((h (make-hash-table :test #'equal)))
                              (setf (gethash "query" h) (%gql-query-text pending conn))
                              h))
                      (response (api-request :post "graphql" :conn conn :body body))
                      (errors   (and (hash-table-p response) (gethash "errors" response)))
                      (data     (and (hash-table-p response)
                                     (let ((d (gethash "data" response)))
                                       (and (hash-table-p d)
                                            (let ((u (gethash "uiapi" d)))
                                              (and (hash-table-p u) (gethash "query" u))))))))
                 (when (and errors (plusp (length errors)))
                   (error 'sf-api-error :message (format nil "GraphQL: ~A" (%gql-errors-text errors))))
                 (unless (hash-table-p data)
                   (error 'sf-api-error :message "GraphQL: the response has no data"))
                 (setf pending
                       (loop for state in pending
                             for page = (gethash (gqs-key state) data)
                             do (unless (hash-table-p page)
                                  (error 'sf-api-error
                                         :message (format nil "GraphQL: no result for ~A" (gqs-key state))))
                                (let ((type-name (gqt-name (gqs-type state))))
                                  (push (map 'vector
                                             (lambda (edge)
                                               (hash->sobject (%gql-flatten (gethash "node" edge) (gqs-type state) conn)
                                                              type-name conn))
                                             (gethash "edges" page))
                                        (gqs-pages state)))
                             when (let ((info (gethash "pageInfo" page)))
                                    (and (not (gqs-limited-p state))
                                         (hash-table-p info)
                                         (gethash "hasNextPage" info)
                                         (setf (gqs-cursor state) (gethash "endCursor" info))))
                               collect state))))
      (let ((result (make-hash-table :test #'equal)))
        (dolist (state states result)
          (let* ((pages (reverse (gqs-pages state)))
                 (out   (make-array (reduce #'+ pages :key #'length)))
                 (i 0))
            (dolist (page pages)
              (replace out page :start1 i)
              (incf i (length page)))
            (setf (gethash (gqs-key state) result) out)))))))

(defmacro gql (&body roots &environment env)
  "Query Salesforce's GraphQL API.  Each ROOT is (ObjectName [:as alias]
   [arguments] selection...), mirroring GraphQL with s-expressions.  Returns
   a hash table (EQUAL) from each object name — or its :AS alias — to a
   vector of CLOS records, with every page fetched.  A GraphQL error
   signals SF-API-ERROR.

   Selections
     Id                   a bare symbol selects that field
     (Name value)         (Field sub-selection...) is Field { ... }
     (Owner (Name value)) a parent record; (Contacts :first 5 (edges (node ...)))
                          a child list, which takes its own arguments
     (:on User (Name value))  an inline fragment, needed for a polymorphic
                          lookup such as Lead.Owner
   Arguments (:key value, before the selections)
     :where     (Industry (in \"Tech\" \"Finance\"))      one field, operators
                (Amount (gte 100) (lt 500))             several operators
                (Account (Industry (eq \"Tech\")))        through a parent
                (and f...) (or f...) (not f)
                operators: eq ne gt gte lt lte in nin like includes excludes
     :order-by  (Name :desc) · (Name :desc :nulls :last) · ((Name :desc) (Id :asc))
     :first n   :after cursor   and any other GraphQL argument (:scope :mine,
                :upper-bound 100 → upperBound)
     :as alias  the result key, and the GraphQL alias (a string is used as written)
   Values: strings, numbers, :keywords (enums), t, nil (null), true/false/null,
   a variable in scope, or any Lisp expression — all escaped; (:raw \"...\")
   inserts GraphQL text as written.  Names: bare symbols in any case (resolved
   from the org's metadata); a string is used exactly as written.

   A top-level query without :first is fetched in full, *GQL-PAGE-SIZE* at a
   time and up to *GQL-UPPER-BOUND* records (or its own :upper-bound n); with
   :first n (at most 2000) it is exactly one page.  Date and datetime fields
   take (gte \"2024-01-01T00:00:00Z\") or a timestamp, a literal such as
   (gte :last-week), or (gte (:range :last-n-days 7)).

   Example
     (with-conn \"myorg\"
       (let ((min-revenue 1000000))
         (gethash \"Account\"
           (gql
             (Account :where (and (Industry (in \"Tech\" \"Finance\"))
                                  (AnnualRevenue (gt min-revenue)))
                      :order-by (Name :asc)
               (edges (node
                 Id (Name value)
                 (Owner (Name value))
                 (Contacts :first 5 (edges (node (LastName value)))))))))))"
  `(%gql-run (list ,@(mapcar (lambda (root) (%gql-root-form root env)) roots))))
