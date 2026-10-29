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
;;; %csv-select — byte-based CSV parser, builds only the named columns
;;; ──────────────────────────────────────────────────────────────────

(defun octets (string)
  (sb-ext:string-to-octets string :external-format :utf-8))

(defun csv-rows (string)
  "STRING's data rows, every column, as lists of strings."
  (let ((header (nth-value 1 (cl-sf/bulk::%csv-select (octets string) nil))))
    (mapcar (lambda (row) (coerce row 'list))
            (cl-sf/bulk::%csv-select (octets string) header))))

(define-test csv-select/header-and-selected-columns
  :parent bulk
  (multiple-value-bind (rows header)
      (cl-sf/bulk::%csv-select
       (octets (format nil "sf__Id,sf__Created,Name~%\"001xx\",\"true\",\"A\"~%\"002xx\",\"false\",\"B\"~%"))
       '("Name" "sf__Id"))
    (is equal '("sf__Id" "sf__Created" "Name") header)
    (is = 2 (length rows))
    (is equalp #("A" "001xx") (first rows))
    (is equalp #("B" "002xx") (second rows))))

(define-test csv-select/unknown-column-is-nil-and-short-row-is-empty
  :parent bulk
  (let ((rows (cl-sf/bulk::%csv-select (octets (format nil "a,b,c~%1,2~%")) '("c" "b" "nope"))))
    (is equalp #("" "2" nil) (first rows))))

(define-test csv-select/empty-and-header-only
  :parent bulk
  (multiple-value-bind (rows header) (cl-sf/bulk::%csv-select (octets "") '("a"))
    (false rows) (false header))
  (multiple-value-bind (rows header) (cl-sf/bulk::%csv-select (octets "sf__Id,sf__Created") '("sf__Id"))
    (false rows)
    (is equal '("sf__Id" "sf__Created") header)))

(define-test csv-select/rfc4180-edge-cases
  :parent bulk
  (flet ((check (expected string) (is equal expected (csv-rows string))))
    (check '(("a" "he said \"hi\"" "c") ("d" "e" "f"))
           (format nil "x,y,z~%a,\"he said \"\"hi\"\"\",c~%d,e,f~%"))
    (check '(("a" "b" "") ("c" "d" ""))
           (format nil "x,y,z~%a,b,~%c,d,~%"))
    (check '(("" "" "") ("x" "" "y"))
           (format nil "p,q,r~%,,~%x,,y~%"))
    (check '(("a,b" "x
y") ("z" "w"))
           (format nil "p,q~%\"a,b\",\"x~%y\"~%z,w~%"))
    (check '(("a" "") ("c" "d"))
           (format nil "p,q~%\"a\",\"\"~%c,d~%"))
    (check '(("a" "b")) (format nil "p,q~%a,b"))                ; no final newline
    (check '(("a" "b")) (format nil "p,q~%\"a\",\"b\""))         ; ... quoted
    (check '(("a" "b") ("c" "d")) (format nil "p,q~%a,b~%~%c,d~%"))   ; blank line
    (check '(("café" "日本") ("naïve ☃" "x"))
           (format nil "p,q~%café,日本~%\"naïve ☃\",x~%"))))

(define-test csv-select/crlf-line-endings
  :parent bulk
  ;; NOTE: "\r\n" in a CL string literal is NOT a CRLF escape; build it.
  (is equal '(("001" "Acme") ("002" "Beta"))
      (csv-rows (format nil "Id,Name~C~C\"001\",\"Acme\"~C~C002,Beta~C~C"
                        #\return #\newline #\return #\newline #\return #\newline))))

(define-test csv-select/structure-spanning-simd-style-block-boundaries
  :parent bulk
  ;; quotes / escaped quotes / commas at every offset around 32 and 64 bytes
  (dotimes (pad 70)
    (let ((filler (make-string pad :initial-element #\x)))
      (is equal (list (list (concatenate 'string filler "a\"b,c") "z"))
          (csv-rows (format nil "p,q~%\"~Aa\"\"b,c\",z~%" filler))))))

(define-test csv-select/round-trips-what-sobjects->csv-writes
  :parent bulk
  (let* ((labels (list "plain" "with, comma" (format nil "multi~%line") "say \"hi\"" "" "café ☃" "tail,\""))
         (objs   (mapcar (lambda (l) (make-widget :label l :amount 1)) labels))
         (csv    (cl-sf/bulk::%sobjects->csv objs '("Label__c" "Amount__c"))))
    (is equal labels
        (map 'list (lambda (row) (svref row 0))
             (cl-sf/bulk::%csv-select (octets csv) '("Label__c"))))))

(define-test csv-select/large-file
  :parent bulk
  (let* ((n 3000)
         (csv (with-output-to-string (o)
                (format o "Id,Name,Note~%")
                (dotimes (i n) (format o "\"~D\",\"name ~D\",\"a,b~D\"~%" i i i))))
         (rows (cl-sf/bulk::%csv-select (octets csv) '("Note" "Id"))))
    (is = n (length rows))
    (is equalp #("a,b2999" "2999") (car (last rows)))))

;;; ──────────────────────────────────────────────────────────────────
;;; %build-bulk-result — correlating result CSVs back to input objects
;;; ──────────────────────────────────────────────────────────────────

(defmacro with-stubbed-results ((&key success failed unprocessed) &body body)
  `(let ((saved (mapcar #'fdefinition '(cl-sf/bulk::get-successful-results
                                        cl-sf/bulk::get-failed-results
                                        cl-sf/bulk::get-unprocessed-results))))
     (unwind-protect
          (progn
            (setf (fdefinition 'cl-sf/bulk::get-successful-results)
                  (lambda (job-id &key conn) (declare (ignore job-id conn)) (octets ,success))
                  (fdefinition 'cl-sf/bulk::get-failed-results)
                  (lambda (job-id &key conn) (declare (ignore job-id conn)) (octets ,failed))
                  (fdefinition 'cl-sf/bulk::get-unprocessed-results)
                  (lambda (job-id &key conn) (declare (ignore job-id conn)) (octets ,unprocessed)))
            ,@body)
       (setf (fdefinition 'cl-sf/bulk::get-successful-results) (first saved)
             (fdefinition 'cl-sf/bulk::get-failed-results) (second saved)
             (fdefinition 'cl-sf/bulk::get-unprocessed-results) (third saved)))))

(define-test build-bulk-result/update-matches-inputs-by-id
  :parent bulk
  (let* ((a (make-instance 'cl-sf/objects:sobject :id "001A" :type "Account"))
         (b (make-instance 'cl-sf/objects:sobject :id "001B" :type "Account"))
         (c (make-instance 'cl-sf/objects:sobject :id "001C" :type "Account")))
    (with-stubbed-results
        (:success (format nil "\"sf__Id\",\"sf__Created\",\"Name\"~%\"001A\",\"false\",\"x\"~%")
         :failed (format nil "\"sf__Id\",\"sf__Error\",\"Name\"~%\"001B\",\"REQUIRED: oops, bad\",\"y\"~%")
         :unprocessed (format nil "\"Id\",\"Name\"~%\"001C\",\"z\"~%"))
      (let ((result (cl-sf/bulk::%build-bulk-result nil "JOB1" (list a b c) :update "Account")))
        (is string= "JOB1" (cl-sf/bulk:bulk-result-job-id result))
        (is equal (list a) (cl-sf/bulk:bulk-result-successful result))
        (is equal (list (cons b "REQUIRED: oops, bad")) (cl-sf/bulk:bulk-result-failed result))
        (is = 1 (length (cl-sf/bulk:bulk-result-unprocessed result)))
        (is string= "001C" (cl-sf/objects:sobject-id (first (cl-sf/bulk:bulk-result-unprocessed result))))))))

(define-test build-bulk-result/insert-makes-fresh-objects-and-failures-have-no-input
  :parent bulk
  (with-stubbed-results
      (:success (format nil "\"sf__Id\",\"sf__Created\"~%\"001N\",\"true\"~%\"001M\",\"true\"~%")
       :failed (format nil "\"sf__Id\",\"sf__Error\"~%\"\",\"FIELD_INTEGRITY: nope\"~%")
       :unprocessed "")
    (let ((result (cl-sf/bulk::%build-bulk-result nil "JOB2" (list (make-widget :label "a")) :insert "Widget")))
      (is equal '("001N" "001M")
          (mapcar #'cl-sf/objects:sobject-id (cl-sf/bulk:bulk-result-successful result)))
      (is string= "Widget" (cl-sf/objects:sobject-type (first (cl-sf/bulk:bulk-result-successful result))))
      (is equal '((nil . "FIELD_INTEGRITY: nope")) (cl-sf/bulk:bulk-result-failed result))
      (false (cl-sf/bulk:bulk-result-unprocessed result)))))

(define-test build-bulk-result/upsert-created-flag-picks-fresh-or-input
  :parent bulk
  (let ((existing (make-instance 'cl-sf/objects:sobject :id "001E" :type "Account")))
    (with-stubbed-results
        (:success (format nil "\"sf__Id\",\"sf__Created\"~%\"001E\",\"false\"~%\"001N\",\"true\"~%")
         :failed "" :unprocessed "")
      (let* ((result (cl-sf/bulk::%build-bulk-result nil "JOB3" (list existing) :upsert "Account"))
             (ok     (cl-sf/bulk:bulk-result-successful result)))
        (true (eq existing (first ok)))
        (is string= "001N" (cl-sf/objects:sobject-id (second ok)))
        (false (eq existing (second ok)))))))

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
