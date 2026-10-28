;;;; cl-sf/tests/bulk-tests.lisp
;;;; Unit tests for cl-sf/bulk — pure CSV and struct functions, no HTTP/lparallel.
;;;; All %sobjects->csv tests use n <= *bulk-csv-chunk-size* to stay in the
;;;; sequential path and avoid needing a lparallel kernel.

(in-package #:cl-sf/tests)

;;; ── Test fixture class ────────────────────────────────────────────
;;; Define a simple sObject class for use in bulk CSV tests.

(cl-sf/objects:define-sobject-class bulk-test-widget "BulkTestWidget__c"
  (label   "Label__c"   :type (or string null))
  (amount  "Amount__c"  :type (or number null))
  (active  "Active__c"  :type (or string null)))

(define-test bulk
  :parent cl-sf/tests
  :description "Bulk 2.0 — CSV generation, parsing, column selection, result struct.")

;;; ──────────────────────────────────────────────────────────────────
;;; %csv-needs-quoting-p
;;; ──────────────────────────────────────────────────────────────────

(define-test csv-quoting/plain-string
  :parent bulk
  (false (cl-sf/bulk::%csv-needs-quoting-p "HelloWorld") "plain string → no quoting"))

(define-test csv-quoting/empty-string
  :parent bulk
  (false (cl-sf/bulk::%csv-needs-quoting-p "") "empty string → no quoting"))

(define-test csv-quoting/contains-comma
  :parent bulk
  (true (cl-sf/bulk::%csv-needs-quoting-p "hello,world") "comma → needs quoting"))

(define-test csv-quoting/contains-double-quote
  :parent bulk
  (true (cl-sf/bulk::%csv-needs-quoting-p "say \"hi\"") "double-quote → needs quoting"))

(define-test csv-quoting/contains-newline
  :parent bulk
  (true (cl-sf/bulk::%csv-needs-quoting-p "line1
line2") "newline → needs quoting"))

(define-test csv-quoting/contains-carriage-return
  :parent bulk
  (true (cl-sf/bulk::%csv-needs-quoting-p (coerce '(#\a #\return #\b) 'string))
        "carriage return → needs quoting"))

;;; ──────────────────────────────────────────────────────────────────
;;; %write-csv-field (via csv-field helper)
;;; ──────────────────────────────────────────────────────────────────

(define-test write-csv-field/plain-string
  :parent bulk
  (is string= "hello" (csv-field "hello")))

(define-test write-csv-field/nil
  :parent bulk
  (is string= "" (csv-field nil) "nil → empty field"))

(define-test write-csv-field/integer
  :parent bulk
  (is string= "42" (csv-field 42)))

(define-test write-csv-field/float
  :parent bulk
  (is string= "3.14" (csv-field 3.14)))

(define-test write-csv-field/string-with-comma
  :parent bulk
  (is string= "\"hello,world\"" (csv-field "hello,world")
      "comma → wrapped in double-quotes"))

(define-test write-csv-field/string-with-embedded-quote
  :parent bulk
  (is string= "\"say \"\"hi\"\"\"" (csv-field "say \"hi\"")
      "double-quote → escaped as \"\" inside quoted field"))

(define-test write-csv-field/string-with-newline
  :parent bulk
  (let ((result (csv-field (format nil "line1~%line2"))))
    (true (char= #\" (char result 0)) "newline-containing value is quoted")))

;;; ──────────────────────────────────────────────────────────────────
;;; %csv-parse-line
;;; ──────────────────────────────────────────────────────────────────

(define-test csv-parse-line/simple-fields
  :parent bulk
  (is equal '("a" "b" "c")
      (cl-sf/bulk::%csv-parse-line "a,b,c")))

(define-test csv-parse-line/quoted-fields
  :parent bulk
  (is equal '("hello" "world")
      (cl-sf/bulk::%csv-parse-line "\"hello\",world")))

(define-test csv-parse-line/embedded-quote
  :parent bulk
  (is equal '("say \"hi\"" "ok")
      (cl-sf/bulk::%csv-parse-line "\"say \"\"hi\"\"\",ok")
      "\"\" inside quoted field → single double-quote"))

(define-test csv-parse-line/empty-middle-field
  :parent bulk
  (is equal '("a" "" "b")
      (cl-sf/bulk::%csv-parse-line "a,,b")
      "consecutive commas → empty string field"))

(define-test csv-parse-line/trailing-comma
  :parent bulk
  (is equal '("a" "b" "")
      (cl-sf/bulk::%csv-parse-line "a,b,")
      "trailing comma → final empty field"))

(define-test csv-parse-line/single-field
  :parent bulk
  (is equal '("only")
      (cl-sf/bulk::%csv-parse-line "only")))

(define-test csv-parse-line/all-quoted
  :parent bulk
  (is equal '("001xx" "true")
      (cl-sf/bulk::%csv-parse-line "\"001xx\",\"true\"")))

;;; ──────────────────────────────────────────────────────────────────
;;; %csv-parse
;;; ──────────────────────────────────────────────────────────────────

(define-test csv-parse/header-and-rows
  :parent bulk
  (multiple-value-bind (header rows)
      (cl-sf/bulk::%csv-parse "sf__Id,sf__Created
\"001xx\",\"true\"
\"002xx\",\"false\"")
    (is equal '("sf__Id" "sf__Created") header)
    (is = 2 (length rows))
    (is string= "001xx" (cdr (assoc "sf__Id"      (first rows) :test #'equal)))
    (is string= "true"  (cdr (assoc "sf__Created" (first rows) :test #'equal)))))

(define-test csv-parse/empty-string
  :parent bulk
  (multiple-value-bind (header rows)
      (cl-sf/bulk::%csv-parse "")
    (false header)
    (false rows)))

(define-test csv-parse/header-only
  :parent bulk
  (multiple-value-bind (header rows)
      (cl-sf/bulk::%csv-parse "sf__Id,sf__Created")
    (is equal '("sf__Id" "sf__Created") header)
    (false rows "No data rows")))

(define-test csv-parse/crlf-line-endings
  :parent bulk
  ;; NOTE: "\r\n" in a CL string literal is NOT a CRLF escape — backslash only
  ;; escapes the next literal character, so that would read as "rn".  Build
  ;; actual CR/LF via #\return #\newline.
  (multiple-value-bind (header rows)
      (cl-sf/bulk::%csv-parse
       (format nil "Id,Name~C~C\"001\",\"Acme\"~C~C"
               #\return #\newline #\return #\newline))
    (is = 1 (length rows))
    (is string= "Acme" (cdr (assoc "Name" (first rows) :test #'equal)))))

;;; ──────────────────────────────────────────────────────────────────
;;; %bulk-column-names
;;; ──────────────────────────────────────────────────────────────────

(defun make-widget (&key label amount active)
  (make-instance 'bulk-test-widget :label label :amount amount :active active))

(define-test bulk-column-names/insert-excludes-id
  :parent bulk
  (let ((cols (cl-sf/bulk::%bulk-column-names (list (make-widget :label "X")) :insert)))
    (false (member "Id" cols :test #'equal) "Id excluded from insert columns")
    (true  (member "Label__c" cols :test #'equal) "Label__c present")))

(define-test bulk-column-names/update-includes-id-first
  :parent bulk
  (let ((cols (cl-sf/bulk::%bulk-column-names (list (make-widget :label "X")) :update)))
    (is string= "Id" (first cols) "Id must be first column for update")))

(define-test bulk-column-names/delete-only-id
  :parent bulk
  (let ((cols (cl-sf/bulk::%bulk-column-names (list (make-widget)) :delete)))
    (is equal '("Id") cols "Delete uses only Id column")))

(define-test bulk-column-names/upsert-keeps-ext-id-field
  :parent bulk
  ;; Label__c is the external id — should appear even if it might be read-only
  (let ((cols (cl-sf/bulk::%bulk-column-names
               (list (make-widget :label "X")) :upsert "Label__c")))
    (true (member "Label__c" cols :test #'equal)
          "External Id field included in upsert columns")))

;;; ──────────────────────────────────────────────────────────────────
;;; %sobjects->csv
;;; ──────────────────────────────────────────────────────────────────

(define-test sobjects->csv/header-row
  :parent bulk
  (let* ((objs  (list (make-widget :label "Foo" :amount 100)))
         (cols  '("Label__c" "Amount__c"))
         (csv   (cl-sf/bulk::%sobjects->csv objs cols)))
    (is string= "Label__c,Amount__c" (first (cl-ppcre:split "\\n" csv))
        "First line is the header")))

(define-test sobjects->csv/data-row
  :parent bulk
  (let* ((objs (list (make-widget :label "Acme" :amount 999)))
         (cols '("Label__c" "Amount__c"))
         (csv  (cl-sf/bulk::%sobjects->csv objs cols))
         (lines (remove "" (cl-ppcre:split "\\n" csv) :test #'equal)))
    (is = 2 (length lines) "One header + one data row")
    (is string= "Acme,999" (second lines))))

(define-test sobjects->csv/nil-field-is-empty
  :parent bulk
  (let* ((objs  (list (make-widget :label "Test" :amount nil)))
         (cols  '("Label__c" "Amount__c"))
         (csv   (cl-sf/bulk::%sobjects->csv objs cols))
         (lines (remove "" (cl-ppcre:split "\\n" csv) :test #'equal)))
    (is string= "Test," (second lines)
        "nil amount → empty column in CSV")))

(define-test sobjects->csv/special-chars-quoted
  :parent bulk
  (let* ((objs  (list (make-widget :label "Hello, World")))
         (cols  '("Label__c"))
         (csv   (cl-sf/bulk::%sobjects->csv objs cols))
         (lines (remove "" (cl-ppcre:split "\\n" csv) :test #'equal)))
    (is string= "\"Hello, World\"" (second lines)
        "Value with comma is quoted")))

(define-test sobjects->csv/multiple-records
  :parent bulk
  (let* ((objs  (list (make-widget :label "A") (make-widget :label "B") (make-widget :label "C")))
         (cols  '("Label__c"))
         (csv   (cl-sf/bulk::%sobjects->csv objs cols))
         (lines (remove "" (cl-ppcre:split "\\n" csv) :test #'equal)))
    (is = 4 (length lines) "Header + 3 data rows")))

(define-test sobjects->csv/id-from-generic-sobject-with-no-raw
  :parent bulk
  ;; Regression: a GENERIC sobject with only :id set and no :raw/slot-map
  ;; (exactly what bulk-insert's own successful results look like) must
  ;; still produce a real Id in a :delete-style CSV, not a blank column —
  ;; feeding bulk-insert's results straight into bulk-delete is normal.
  (let* ((objs  (list (make-instance 'cl-sf/objects:sobject :id "001xxBULKTEST" :type "Account")))
         (cols  '("Id"))
         (csv   (cl-sf/bulk::%sobjects->csv objs cols))
         (lines (remove "" (cl-ppcre:split "\\n" csv) :test #'equal)))
    (is string= "001xxBULKTEST" (second lines)
        "Id column is populated from SOBJECT-ID, not left blank")))

;;; ──────────────────────────────────────────────────────────────────
;;; bulk-result struct
;;; ──────────────────────────────────────────────────────────────────

(define-test bulk-result/struct-creation
  :parent bulk
  (let ((result (cl-sf/bulk:make-bulk-result
                 :successful  '(1 2 3)
                 :failed      '((nil . "err"))
                 :unprocessed '()
                 :job-id      "7500x0000001")))
    (is equal '(1 2 3)          (cl-sf/bulk:bulk-result-successful  result))
    (is equal '((nil . "err"))  (cl-sf/bulk:bulk-result-failed      result))
    (false                       (cl-sf/bulk:bulk-result-unprocessed result))
    (is string= "7500x0000001"  (cl-sf/bulk:bulk-result-job-id      result))))

(define-test bulk-result/p-predicate
  :parent bulk
  (let ((r (cl-sf/bulk:make-bulk-result)))
    (true  (cl-sf/bulk:bulk-result-p r) "bulk-result-p T for bulk-result instance")
    (false (cl-sf/bulk:bulk-result-p "not a bulk result"))))

;;; ──────────────────────────────────────────────────────────────────
;;; Conditions
;;; ──────────────────────────────────────────────────────────────────

(define-test bulk-error/signal-and-catch
  :parent bulk
  (handler-case
      (error 'cl-sf/bulk:bulk-error :message "test")
    (cl-sf/bulk:bulk-error (e)
      (is string= "test" (cl-sf/bulk:bulk-error-message e)))))

(define-test bulk-timeout-error/accessors
  :parent bulk
  (handler-case
      (error 'cl-sf/bulk:bulk-timeout-error
             :job-id "JOB1" :timeout 60 :message "timed out")
    (cl-sf/bulk:bulk-timeout-error (e)
      (is string= "JOB1" (cl-sf/bulk:bulk-timeout-error-job-id e))
      (is = 60 (cl-sf/bulk:bulk-timeout-error-timeout e)))))

(define-test bulk-job-failed-error/accessors
  :parent bulk
  (let ((status (make-hash "state" "Failed")))
    (handler-case
        (error 'cl-sf/bulk:bulk-job-failed-error
               :job-id "JOB2" :status status :message "failed")
      (cl-sf/bulk:bulk-job-failed-error (e)
        (is string= "JOB2" (cl-sf/bulk:bulk-job-failed-error-job-id e))
        (is equal status   (cl-sf/bulk:bulk-job-failed-error-status e))))))
