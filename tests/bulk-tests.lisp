;;;; cl-sf/tests/bulk-tests.lisp
;;;; Unit tests for cl-sf/bulk — CSV writing/parsing, result correlation (HTTP stubbed).

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
;;; %write-field (via csv-field helper)
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
             (cl-sf/bulk::%csv-select csv '("Label__c"))))))

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
         (csv   (csv-text (cl-sf/bulk::%sobjects->csv objs cols))))
    (is string= "Label__c,Amount__c" (first (cl-ppcre:split "\\n" csv))
        "First line is the header")))

(define-test sobjects->csv/data-row
  :parent bulk
  (let* ((objs (list (make-widget :label "Acme" :amount 999)))
         (cols '("Label__c" "Amount__c"))
         (csv  (csv-text (cl-sf/bulk::%sobjects->csv objs cols)))
         (lines (remove "" (cl-ppcre:split "\\n" csv) :test #'equal)))
    (is = 2 (length lines) "One header + one data row")
    (is string= "Acme,999" (second lines))))

(define-test sobjects->csv/nil-field-is-empty
  :parent bulk
  (let* ((objs  (list (make-widget :label "Test" :amount nil)))
         (cols  '("Label__c" "Amount__c"))
         (csv   (csv-text (cl-sf/bulk::%sobjects->csv objs cols)))
         (lines (remove "" (cl-ppcre:split "\\n" csv) :test #'equal)))
    (is string= "Test," (second lines)
        "nil amount → empty column in CSV")))

(define-test sobjects->csv/special-chars-quoted
  :parent bulk
  (let* ((objs  (list (make-widget :label "Hello, World")))
         (cols  '("Label__c"))
         (csv   (csv-text (cl-sf/bulk::%sobjects->csv objs cols)))
         (lines (remove "" (cl-ppcre:split "\\n" csv) :test #'equal)))
    (is string= "\"Hello, World\"" (second lines)
        "Value with comma is quoted")))

(define-test sobjects->csv/multiple-records
  :parent bulk
  (let* ((objs  (list (make-widget :label "A") (make-widget :label "B") (make-widget :label "C")))
         (cols  '("Label__c"))
         (csv   (csv-text (cl-sf/bulk::%sobjects->csv objs cols)))
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
         (csv   (csv-text (cl-sf/bulk::%sobjects->csv objs cols)))
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

;;; ──────────────────────────────────────────────────────────────────
;;; CSV writer — UTF-8 octets, column plan, chunked parallel path
;;; ──────────────────────────────────────────────────────────────────

(define-test write-field/integers-without-consing-paths
  :parent bulk
  (is string= "0" (csv-field 0))
  (is string= "9" (csv-field 9))
  (is string= "10" (csv-field 10))
  (is string= "1234567890" (csv-field 1234567890))
  (is string= (princ-to-string most-positive-fixnum) (csv-field most-positive-fixnum))
  (is string= "-5" (csv-field -5))
  (is string= (princ-to-string (expt 10 30)) (csv-field (expt 10 30))))

(define-test write-field/floats-booleans-and-symbols
  :parent bulk
  (is string= "1.5" (csv-field 1.5d0))
  (is string= "0.1" (csv-field 0.1))
  (is string= "true" (csv-field t))
  (is string= "" (csv-field nil))
  (is string= "FOO" (csv-field :foo))
  (is string= "1/3" (csv-field 1/3)))

(define-test write-field/non-ascii-is-utf-8
  :parent bulk
  (flet ((bytes (value)
           (let ((buf (cl-sf/bulk::%make-obuf 4)))
             (cl-sf/bulk::%write-field buf value)
             (coerce (cl-sf/bulk::%obuf-octets buf) 'list))))
    (is equal '(#x63 #x61 #x66 #xC3 #xA9) (bytes "café"))            ; 2-byte
    (is equal '(#xE2 #x98 #x83) (bytes (string (code-char #x2603))))        ; 3-byte
    (is equal '(#xF0 #x9F #x98 #x80) (bytes (string (code-char #x1F600))))  ; 4-byte
    ;; quoted, with a doubled quote, and a multi-byte char after it
    (is equal '(34 97 34 34 #xC3 #xA9 34) (bytes (format nil "a\"é")))))

(define-test write-field/buffer-grows-across-many-fields
  :parent bulk
  ;; each cell is "x,y" quoted (5 bytes) plus a newline, 1000 times
  (let ((buf (cl-sf/bulk::%make-obuf 1)))
    (dotimes (i 1000)
      (cl-sf/bulk::%write-field buf "x,y")
      (cl-sf/bulk::%obuf-byte buf 10))
    (is = 6000 (length (cl-sf/bulk::%obuf-octets buf)))))

(define-test sobjects->csv/returns-exact-size-octets
  :parent bulk
  (let ((csv (cl-sf/bulk::%sobjects->csv (list (make-widget :label "Acme" :amount 5)) '("Label__c" "Amount__c"))))
    (true (typep csv '(simple-array (unsigned-byte 8) (*))))
    (is string= (format nil "Label__c,Amount__c~%Acme,5~%") (csv-text csv))))

(define-test sobjects->csv/empty-list-is-just-the-header
  :parent bulk
  (is string= (format nil "Id~%") (csv-text (cl-sf/bulk::%sobjects->csv '() '("Id")))))

(define-test sobjects->csv/parallel-chunks-match-the-sequential-output
  :parent bulk
  (let* ((objs (loop for i below 53
                     collect (make-widget :label (format nil "n~D, \"q\" café ~D" i i) :amount i)))
         (cols '("Label__c" "Amount__c"))
         (sequential (let ((cl-sf/bulk:*bulk-csv-chunk-size* 100000))
                       (cl-sf/bulk::%sobjects->csv objs cols)))
         (parallel   (let ((cl-sf/bulk:*bulk-csv-chunk-size* 7))
                       (cl-sf/bulk::%sobjects->csv objs cols))))
    (is = 54 (count 10 sequential))
    (is equalp sequential parallel)))

(define-test sobjects->csv/id-slot-and-raw-columns
  :parent bulk
  (let* ((typed   (make-widget :label "T"))
         (raw     (make-hash-table :test #'equal))
         (generic (make-instance 'cl-sf/objects:sobject :id "001G" :type "X" :raw raw)))
    (setf (cl-sf/objects:sobject-id typed) "001T"
          (gethash "Custom__c" raw) "from raw"
          (gethash "Id" raw) "ignored: Id comes from the accessor")
    ;; typed object: Id accessor, typed slot, and a field the slot map lacks (-> raw)
    (is string= (format nil "Id,Label__c,Missing__c~%001T,T,~%")
        (csv-text (cl-sf/bulk::%sobjects->csv (list typed) '("Id" "Label__c" "Missing__c"))))
    ;; generic object: everything from raw, Id still from the accessor
    (is string= (format nil "Id,Custom__c~%001G,from raw~%")
        (csv-text (cl-sf/bulk::%sobjects->csv (list generic) '("Id" "Custom__c"))))))

(define-test sobjects->csv/records-of-another-class-take-the-generic-path
  :parent bulk
  ;; the plan is built from the FIRST record; a record of a different class
  ;; must not be read through that class's slot indexes
  (let* ((widget (make-widget :label "W" :amount 1))
         (other  (make-instance 'test-contact :last-name "Lopez")))
    (is string= (format nil "Label__c~%W~%~%")
        (csv-text (cl-sf/bulk::%sobjects->csv (list widget other) '("Label__c"))))))

(define-test sobjects->csv/unbound-slot-is-an-empty-cell
  :parent bulk
  (let ((w (make-widget :label "L")))
    (slot-makunbound w 'cl-sf/tests::label)
    (is string= (format nil "Label__c~%~%")
        (csv-text (cl-sf/bulk::%sobjects->csv (list w) '("Label__c"))))))

;;; ──────────────────────────────────────────────────────────────────
;;; Bulk latency: polling backoff, selective/parallel result downloads,
;;; job creation overlapped with CSV generation (HTTP stubbed)
;;; ──────────────────────────────────────────────────────────────────

(defvar *bulk-log-lock* (sb-thread:make-mutex :name "bulk test log"))

(defmacro bulk-log (item log)
  `(sb-thread:with-mutex (*bulk-log-lock*) (push ,item ,log)))

(defmacro with-fdefinitions ((&rest bindings) &body body)
  "Temporarily replace each (NAME FUNCTION) global function binding."
  `(let ((saved (mapcar (lambda (name) (fdefinition name)) ',(mapcar #'first bindings))))
     (unwind-protect
          (progn ,@(loop for (name fn) in bindings
                         collect `(setf (fdefinition ',name) ,fn))
                 ,@body)
       (loop for name in ',(mapcar #'first bindings)
             for fn in saved
             do (setf (fdefinition name) fn)))))

(defun job-status (state &key processed failed)
  (let ((h (make-hash-table :test #'equal)))
    (setf (gethash "state" h) state)
    (when processed (setf (gethash "numberRecordsProcessed" h) processed))
    (when failed (setf (gethash "numberRecordsFailed" h) failed))
    h))

;;; ── polling ──

(define-test poll-job/backs-off-up-to-the-interval
  :parent bulk
  (let ((states (list "Open" "UploadComplete" "InProgress" "InProgress" "InProgress" "InProgress"
                      "InProgress" "JobComplete"))
        (sleeps '()))
    (with-fdefinitions ((cl-sf/bulk::get-bulk-job
                         (lambda (job-id &key conn) (declare (ignore job-id conn))
                           (job-status (pop states)))))
      (let ((final (cl-sf/bulk::%poll-job nil "J" :interval 5
                                                  :sleep (lambda (s) (push s sleeps)))))
        (is string= "JobComplete" (gethash "state" final))
        (is equal '(0.5 1.0 2.0 4.0 5 5 5) (reverse sleeps))))))

(define-test poll-job/finished-job-is-not-waited-for
  :parent bulk
  (let ((sleeps '()))
    (with-fdefinitions ((cl-sf/bulk::get-bulk-job
                         (lambda (job-id &key conn) (declare (ignore job-id conn))
                           (job-status "JobComplete"))))
      (cl-sf/bulk::%poll-job nil "J" :sleep (lambda (s) (push s sleeps)))
      (is equal '() sleeps))))

(define-test poll-job/small-interval-caps-the-first-wait
  :parent bulk
  (let ((states (list "InProgress" "JobComplete")) (sleeps '()))
    (with-fdefinitions ((cl-sf/bulk::get-bulk-job
                         (lambda (job-id &key conn) (declare (ignore job-id conn))
                           (job-status (pop states)))))
      (cl-sf/bulk::%poll-job nil "J" :interval 0.1 :sleep (lambda (s) (push s sleeps)))
      (is equal '(0.1) sleeps))))

(define-test poll-job/times-out
  :parent bulk
  (with-fdefinitions ((cl-sf/bulk::get-bulk-job
                       (lambda (job-id &key conn) (declare (ignore job-id conn))
                         (job-status "InProgress"))))
    (fail (cl-sf/bulk::%poll-job nil "J" :timeout 0.05 :sleep (lambda (s) (declare (ignore s)) (sleep 0.02)))
          'cl-sf/bulk:bulk-timeout-error)))

;;; ── which result files, fetched how ──

(define-test results-to-fetch/skips-files-that-must-be-empty
  :parent bulk
  (flet ((kinds (processed failed) (cl-sf/bulk::%results-to-fetch (job-status "JobComplete" :processed processed :failed failed))))
    (is equal '(:successful) (kinds 10 0))
    (is equal '(:successful :failed) (kinds 10 3))
    (is equal '(:failed) (kinds 4 4))
    (is equal '() (kinds 0 0)))
  ;; no counts (or no status at all): fetch everything
  (is equal '(:successful :failed :unprocessed) (cl-sf/bulk::%results-to-fetch (job-status "JobComplete")))
  (is equal '(:successful :failed :unprocessed) (cl-sf/bulk::%results-to-fetch nil)))

(defmacro with-result-downloads ((log &key (success "") (failed "") (unprocessed "") (delay 0)) &body body)
  "Stub the three result downloads: log (kind thread) when called, sleep DELAY."
  `(with-fdefinitions ((cl-sf/bulk::get-successful-results
                        (lambda (job-id &key conn) (declare (ignore job-id conn))
                          (bulk-log (list :successful sb-thread:*current-thread*) ,log)
                          (sleep ,delay) (octets ,success)))
                       (cl-sf/bulk::get-failed-results
                        (lambda (job-id &key conn) (declare (ignore job-id conn))
                          (bulk-log (list :failed sb-thread:*current-thread*) ,log)
                          (sleep ,delay) (octets ,failed)))
                       (cl-sf/bulk::get-unprocessed-results
                        (lambda (job-id &key conn) (declare (ignore job-id conn))
                          (bulk-log (list :unprocessed sb-thread:*current-thread*) ,log)
                          (sleep ,delay) (octets ,unprocessed))))
     ,@body))

(define-test build-bulk-result/downloads-only-what-the-counts-call-for
  :parent bulk
  (let ((log '()))
    (with-result-downloads (log :success (format nil "sf__Id,sf__Created~%001A,false~%"))
      (let ((result (cl-sf/bulk::%build-bulk-result
                     nil "J" (list (make-instance 'cl-sf/objects:sobject :id "001A" :type "Account"))
                     :update "Account" :status (job-status "JobComplete" :processed 1 :failed 0))))
        (is equal '(:successful) (mapcar #'first log))
        (is = 1 (length (cl-sf/bulk:bulk-result-successful result)))
        (false (cl-sf/bulk:bulk-result-failed result))
        (false (cl-sf/bulk:bulk-result-unprocessed result))))))

(define-test build-bulk-result/downloads-run-concurrently
  :parent bulk
  (let ((log '()))
    (with-result-downloads (log :delay 0.3
                                :success (format nil "sf__Id,sf__Created~%001A,true~%")
                                :failed (format nil "sf__Id,sf__Error~%,boom~%"))
      (let* ((start (get-internal-real-time))
             (result (cl-sf/bulk::%build-bulk-result nil "J" '() :insert "Account"
                                                     :status (job-status "JobComplete" :processed 2 :failed 1)))
             (elapsed (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
        (is equal '(:failed :successful) (sort (mapcar #'first log) #'string<))
        (is = 2 (length (remove-duplicates (mapcar #'second log))) "two different worker threads")
        (true (< elapsed 0.55) "two 0.3 s downloads overlapped (serial would be 0.6 s)")
        (is = 1 (length (cl-sf/bulk:bulk-result-successful result)))
        (is equal '((nil . "boom")) (cl-sf/bulk:bulk-result-failed result))))))

;;; ── the whole operation ──

(defun stub-job-id (job) (gethash "id" job))

(defun job (id)
  (let ((h (make-hash-table :test #'equal))) (setf (gethash "id" h) id) h))

(define-test run-bulk-operation/creates-the-job-while-the-csv-is-built
  :parent bulk
  (let ((log '()) (objs (list (make-widget :label "A" :amount 1))))
    (with-fdefinitions ((cl-sf/bulk::create-bulk-job
                         (lambda (type op &key conn external-id-field-name)
                           (declare (ignore type op conn external-id-field-name))
                           (bulk-log :create-start log)
                           (sleep 0.3)
                           (bulk-log :create-end log)
                           (job "J1")))
                        (cl-sf/bulk::%sobjects->csv
                         (lambda (records fields) (declare (ignore records fields))
                           (bulk-log :csv-start log)
                           (sleep 0.05)
                           (bulk-log :csv-end log)
                           (octets "Label__c,Amount__c")))
                        (cl-sf/api:api-request
                         (lambda (method path &key conn body params content-type raw)
                           (declare (ignore conn params content-type raw))
                           (bulk-log (list :put method path (length body)) log) t))
                        (cl-sf/bulk::close-bulk-job
                         (lambda (id &key conn) (declare (ignore conn)) (bulk-log (list :close id) log) t))
                        (cl-sf/bulk::get-bulk-job
                         (lambda (id &key conn) (declare (ignore id conn))
                           (job-status "JobComplete" :processed 1 :failed 0))))
      (with-result-downloads (log :success (format nil "sf__Id,sf__Created~%001N,true~%"))
        (let* ((result (cl-sf/bulk::%run-bulk-operation nil :insert objs))
               (events  (reverse log))
               (pos (lambda (event) (position event events :key (lambda (e) (if (consp e) (car e) e))))))
          (true (< (funcall pos :create-start) (funcall pos :csv-end))
                "job creation started before the CSV was finished")
          (true (< (funcall pos :create-end) (funcall pos :put)) "upload waits for the job")
          (is equal '(:put :post-ignored) (list :put :post-ignored))
          (is string= "001N" (cl-sf/objects:sobject-id (first (cl-sf/bulk:bulk-result-successful result))))
          (is string= "J1" (cl-sf/bulk:bulk-result-job-id result))
          ;; the success-only job downloaded only the successful file
          (is = 1 (count :successful events :key (lambda (e) (if (consp e) (car e) e)))))))))

(define-test run-bulk-operation/a-failed-csv-aborts-the-job-that-was-created
  :parent bulk
  (let ((log '()))
    (with-fdefinitions ((cl-sf/bulk::create-bulk-job
                         (lambda (type op &key conn external-id-field-name)
                           (declare (ignore type op conn external-id-field-name))
                           (sleep 0.1) (job "J9")))
                        (cl-sf/bulk::%sobjects->csv
                         (lambda (records fields) (declare (ignore records fields))
                           (error "cannot serialize")))
                        (cl-sf/bulk::abort-bulk-job
                         (lambda (id &key conn) (declare (ignore conn)) (bulk-log (list :abort id) log) t)))
      (fail (cl-sf/bulk::%run-bulk-operation nil :insert (list (make-widget :label "A"))))
      (is equal '((:abort "J9")) log))))

(define-test run-bulk-operation/a-failed-job-creation-is-the-error-and-nothing-is-aborted
  :parent bulk
  (let ((log '()))
    (with-fdefinitions ((cl-sf/bulk::create-bulk-job
                         (lambda (type op &key conn external-id-field-name)
                           (declare (ignore type op conn external-id-field-name))
                           (error 'cl-sf/api:sf-api-error :message "no create")))
                        (cl-sf/bulk::%sobjects->csv
                         (lambda (records fields) (declare (ignore records fields)) (octets "x")))
                        (cl-sf/bulk::abort-bulk-job
                         (lambda (id &key conn) (declare (ignore conn)) (bulk-log (list :abort id) log) t)))
      (fail (cl-sf/bulk::%run-bulk-operation nil :insert (list (make-widget :label "A")))
            'cl-sf/api:sf-api-error)
      (is equal '() log))))

(define-test run-bulk-operation/a-failed-job-aborts-and-signals
  :parent bulk
  (let ((log '()))
    (with-fdefinitions ((cl-sf/bulk::create-bulk-job
                         (lambda (type op &key conn external-id-field-name)
                           (declare (ignore type op conn external-id-field-name)) (job "J2")))
                        (cl-sf/bulk::%sobjects->csv
                         (lambda (records fields) (declare (ignore records fields)) (octets "x")))
                        (cl-sf/api:api-request
                         (lambda (method path &key conn body params content-type raw)
                           (declare (ignore method path conn body params content-type raw)) t))
                        (cl-sf/bulk::close-bulk-job (lambda (id &key conn) (declare (ignore id conn)) t))
                        (cl-sf/bulk::get-bulk-job
                         (lambda (id &key conn) (declare (ignore id conn)) (job-status "Failed")))
                        (cl-sf/bulk::abort-bulk-job
                         (lambda (id &key conn) (declare (ignore conn)) (bulk-log (list :abort id) log) t)))
      (fail (cl-sf/bulk::%run-bulk-operation nil :insert (list (make-widget :label "A")))
            'cl-sf/bulk:bulk-job-failed-error)
      (is equal '((:abort "J2")) log))))

;;; ── Bulk insert results map back to the objects that were sent ────

(define-test insert-matching
  :parent bulk
  :description "Result rows matched to input objects by the columns Salesforce echoes.")

(defun upload-of (objects columns)
  (cl-sf/bulk::%sobjects->csv objects columns))

(defun escaped (cell)
  (with-output-to-string (out)
    (loop for c across cell do (when (char= c #\") (write-char #\" out)) (write-char c out))))

(defun result-csv (header &rest rows)
  "HEADER (a list of column names) and ROWS (lists of cells) as a quoted CSV string."
  (with-output-to-string (out)
    (flet ((line (cells) (format out "~{\"~A\"~^,~}~%" (mapcar #'escaped cells))))
      (line header)
      (dolist (row rows) (line row)))))

(define-test insert-matching/results-in-any-order-map-to-their-own-objects
  :parent insert-matching
  (let* ((a (make-widget :label "alpha" :amount 1))
         (b (make-widget :label "beta" :amount 2))
         (c (make-widget :label "gamma" :amount 3))
         (columns '("Label__c" "Amount__c"))
         (header (list "sf__Id" "sf__Created" "Label__c" "Amount__c")))
    (with-stubbed-results
        (:success (result-csv header '("001C" "true" "gamma" "3") '("001A" "true" "alpha" "1") '("001B" "true" "beta" "2"))
         :failed "" :unprocessed "")
      (let ((result (cl-sf/bulk::%build-bulk-result nil "J" (list a b c) :insert "Widget"
                                                    :upload (upload-of (list a b c) columns) :columns columns)))
        (is equal '("001A" "001B" "001C") (mapcar #'cl-sf/objects:sobject-id (list a b c)) "Ids landed on the right objects")
        (is = 3 (length (cl-sf/bulk:bulk-result-successful result)))
        (true (every #'eq (cl-sf/bulk:bulk-result-successful result) (list c a b))
              "the result holds the very objects sent, in the order Salesforce answered")))))

(define-test insert-matching/failed-rows-come-back-with-their-objects
  :parent insert-matching
  (let* ((good (make-widget :label "fine" :amount 1))
         (bad  (make-widget :label "broken" :amount 2))
         (columns '("Label__c" "Amount__c")))
    (with-stubbed-results
        (:success (result-csv '("sf__Id" "sf__Created" "Label__c" "Amount__c") '("001G" "true" "fine" "1"))
         :failed (result-csv '("sf__Id" "sf__Error" "Label__c" "Amount__c")
                             '("" "FIELD_CUSTOM_VALIDATION_EXCEPTION:nope" "broken" "2"))
         :unprocessed "")
      (let ((result (cl-sf/bulk::%build-bulk-result nil "J" (list good bad) :insert "Widget"
                                                    :upload (upload-of (list good bad) columns) :columns columns)))
        (is string= "001G" (cl-sf/objects:sobject-id good))
        (false (cl-sf/objects:sobject-id bad) "a failed insert gets no Id")
        (is eq bad (car (first (cl-sf/bulk:bulk-result-failed result))))
        (true (search "FIELD_CUSTOM_VALIDATION_EXCEPTION" (cdr (first (cl-sf/bulk:bulk-result-failed result)))))))))

(define-test insert-matching/identical-rows-each-get-their-own-id
  :parent insert-matching
  (let* ((one (make-widget :label "same" :amount 1))
         (two (make-widget :label "same" :amount 1))
         (columns '("Label__c" "Amount__c")))
    (with-stubbed-results
        (:success (result-csv '("sf__Id" "sf__Created" "Label__c" "Amount__c")
                              '("001X" "true" "same" "1") '("001Y" "true" "same" "1"))
         :failed "" :unprocessed "")
      (let ((result (cl-sf/bulk::%build-bulk-result nil "J" (list one two) :insert "Widget"
                                                    :upload (upload-of (list one two) columns) :columns columns)))
        (is equal '("001X" "001Y") (mapcar #'cl-sf/objects:sobject-id (list one two)))
        (true (and (eq one (first (cl-sf/bulk:bulk-result-successful result)))
                   (eq two (second (cl-sf/bulk:bulk-result-successful result)))))))))

(define-test insert-matching/awkward-values-match
  :parent insert-matching
  (let* ((labels (list "plain" "with, comma" (format nil "multi~%line") "say \"hi\"" "  padded  "
                       "café ☃ 日本" "tail,\"" "" "a:1"))
         (objects (mapcar (lambda (l) (make-widget :label l :amount 7)) labels))
         (columns '("Label__c" "Amount__c")))
    (with-stubbed-results
        (:success (apply #'result-csv '("sf__Id" "sf__Created" "Label__c" "Amount__c")
                         (loop for l in (reverse labels) for i from 0
                               collect (list (format nil "001~D" i) "true" l "7")))
         :failed "" :unprocessed "")
      (let ((result (cl-sf/bulk::%build-bulk-result nil "J" objects :insert "Widget"
                                                    :upload (upload-of objects columns) :columns columns)))
        (is = (length labels) (length (cl-sf/bulk:bulk-result-successful result)))
        (true (every (lambda (o) (member o objects :test #'eq)) (cl-sf/bulk:bulk-result-successful result))
              "every result is one of ours, none a stand-in")
        (is = (length labels) (length (remove nil (mapcar #'cl-sf/objects:sobject-id objects))))))))

(define-test insert-matching/rows-that-do-not-match-fall-back-to-fresh-objects
  :parent insert-matching
  (let* ((one (make-widget :label "sent one" :amount 1))
         (two (make-widget :label "sent two" :amount 2))
         (columns '("Label__c" "Amount__c")))
    (with-stubbed-results
        (:success (result-csv '("sf__Id" "sf__Created" "Label__c" "Amount__c")
                              '("001Y" "true" "CHANGED-2" "2") '("001Z" "true" "CHANGED-1" "1"))
         :failed "" :unprocessed "")
      (let* ((result (cl-sf/bulk::%build-bulk-result nil "J" (list one two) :insert "Widget"
                                                     :upload (upload-of (list one two) columns) :columns columns))
             (stand-ins (cl-sf/bulk:bulk-result-successful result)))
        (is = 2 (length stand-ins))
        (true (notany (lambda (o) (member o (list one two) :test #'eq)) stand-ins))
        (is equal '("001Y" "001Z") (mapcar #'cl-sf/objects:sobject-id stand-ins))
        (is string= "Widget" (cl-sf/objects:sobject-type (first stand-ins)))
        (false (cl-sf/objects:sobject-id one))
        (false (cl-sf/objects:sobject-id two))))))

(define-test insert-matching/numbers-salesforce-rewrites-still-match
  :parent insert-matching
  ;; observed live: Amount sent as 1.50 is echoed as 1.5
  (let* ((a (make-widget :label "a" :amount "1.50"))
         (b (make-widget :label "b" :amount 1000.0d0))
         (c (make-widget :label "c" :amount "+7.0"))
         (columns '("Label__c" "Amount__c")))
    (with-stubbed-results
        (:success (result-csv '("sf__Id" "sf__Created" "Label__c" "Amount__c")
                              '("001A" "true" "a" "1.5") '("001B" "true" "b" "1000") '("001C" "true" "c" "7"))
         :failed "" :unprocessed "")
      (let ((result (cl-sf/bulk::%build-bulk-result nil "J" (list a b c) :insert "Widget"
                                                    :upload (upload-of (list a b c) columns) :columns columns)))
        (is equal '("001A" "001B" "001C") (mapcar #'cl-sf/objects:sobject-id (list a b c)))
        (true (every #'eq (cl-sf/bulk:bulk-result-successful result) (list a b c)))))))

(define-test insert-matching/one-leftover-pair-is-matched-by-elimination
  :parent insert-matching
  ;; the other rows match exactly; the last differs in a way we don't know about
  (let* ((a (make-widget :label "a" :amount 1))
         (b (make-widget :label "b" :amount 2))
         (odd (make-widget :label "odd" :amount 3))
         (columns '("Label__c" "Amount__c")))
    (with-stubbed-results
        (:success (result-csv '("sf__Id" "sf__Created" "Label__c" "Amount__c")
                              '("001A" "true" "a" "1") '("001B" "true" "b" "2") '("001O" "true" "ODD, rewritten" "3"))
         :failed "" :unprocessed "")
      (let ((result (cl-sf/bulk::%build-bulk-result nil "J" (list a b odd) :insert "Widget"
                                                    :upload (upload-of (list a b odd) columns) :columns columns)))
        (is string= "001O" (cl-sf/objects:sobject-id odd))
        (true (every #'eq (cl-sf/bulk:bulk-result-successful result) (list a b odd)))))))

(define-test insert-matching/elimination-needs-every-input-to-have-a-result-row
  :parent insert-matching
  ;; 3 inputs but only 2 result rows (one unaccounted for): one leftover pair would be a guess
  (let* ((a (make-widget :label "a" :amount 1))
         (b (make-widget :label "b" :amount 2))
         (c (make-widget :label "c" :amount 3))
         (columns '("Label__c" "Amount__c")))
    (with-stubbed-results
        (:success (result-csv '("sf__Id" "sf__Created" "Label__c" "Amount__c")
                              '("001A" "true" "a" "1") '("001X" "true" "REWRITTEN" "2"))
         :failed "" :unprocessed "")
      (let ((result (cl-sf/bulk::%build-bulk-result nil "J" (list a b c) :insert "Widget"
                                                    :upload (upload-of (list a b c) columns) :columns columns)))
        (is string= "001A" (cl-sf/objects:sobject-id a))
        (false (cl-sf/objects:sobject-id b))
        (false (cl-sf/objects:sobject-id c))
        (is = 2 (length (cl-sf/bulk:bulk-result-successful result)))))))

(define-test canonical-number/normalises-plain-decimals-only
  :parent insert-matching
  (flet ((canon (s) (cl-sf/bulk::%canonical-number s)))
    (is string= "1.5" (canon "1.50"))
    (is string= "1.5" (canon "+1.5"))
    (is string= "1.5" (canon "001.500"))
    (is string= "1000" (canon "1000.0"))
    (is string= "0" (canon "0.000"))
    (is string= "0" (canon "-0.0"))
    (is string= "-2.5" (canon "-2.50"))
    (is string= "7" (canon "7"))
    ;; not plain decimals: untouched
    (is string= "" (canon ""))
    (is string= "abc" (canon "abc"))
    (is string= "1e5" (canon "1e5"))
    (is string= "1.2.3" (canon "1.2.3"))
    (is string= "-" (canon "-"))
    (is string= "." (canon "."))
    (is string= "12:30" (canon "12:30"))
    (is string= "2030-05-01" (canon "2030-05-01"))))

(define-test insert-matching/without-the-upload-it-is-the-old-behaviour
  :parent insert-matching
  (let ((sent (make-widget :label "sent")))
    (with-stubbed-results
        (:success (result-csv '("sf__Id" "sf__Created") '("001Q" "true")) :failed "" :unprocessed "")
      (let ((result (cl-sf/bulk::%build-bulk-result nil "J" (list sent) :insert "Widget")))
        (false (eq sent (first (cl-sf/bulk:bulk-result-successful result))))
        (false (cl-sf/objects:sobject-id sent))))))

(define-test insert-matching/upsert-rows-without-an-id-are-matched-by-content
  :parent insert-matching
  (let* ((existing (make-widget :label "old" :amount 1))
         (fresh    (make-widget :label "new" :amount 2))
         (columns (cl-sf/bulk::%bulk-column-names (list existing fresh) :upsert "Id")))
    (setf (cl-sf/objects:sobject-id existing) "001OLD")
    (with-stubbed-results
        (:success (result-csv '("sf__Id" "sf__Created" "Id" "Label__c" "Amount__c" "Active__c")
                              '("001NEW" "true" "" "new" "2" "") '("001OLD" "false" "001OLD" "old" "1" ""))
         :failed "" :unprocessed "")
      (let ((result (cl-sf/bulk::%build-bulk-result nil "J" (list existing fresh) :upsert "Widget"
                                                    :upload (upload-of (list existing fresh) columns)
                                                    :columns columns)))
        (true (every #'eq (cl-sf/bulk:bulk-result-successful result) (list fresh existing)))
        (is string= "001NEW" (cl-sf/objects:sobject-id fresh) "the created record's object learns its Id")
        (is string= "001OLD" (cl-sf/objects:sobject-id existing))))))

(define-test insert-matching/update-and-delete-still-match-by-id
  :parent insert-matching
  (let* ((a (make-instance 'cl-sf/objects:sobject :id "001A" :type "Account"))
         (columns '("Id")))
    (with-stubbed-results
        (:success (result-csv '("sf__Id" "sf__Created" "Id") '("001A" "false" "001A")) :failed "" :unprocessed "")
      (let ((result (cl-sf/bulk::%build-bulk-result nil "J" (list a) :delete "Account"
                                                    :upload (upload-of (list a) columns) :columns columns)))
        (is eq a (first (cl-sf/bulk:bulk-result-successful result)))))))

(define-test insert-matching/a-matching-error-still-returns-the-ids
  :parent insert-matching
  (let* ((sent (make-widget :label "sent" :amount 1))
         (columns '("Label__c" "Amount__c"))
         (warned nil))
    (with-stubbed-results
        (:success (result-csv '("sf__Id" "sf__Created" "Label__c" "Amount__c") '("001K" "true" "sent" "1"))
         :failed "" :unprocessed "")
      (with-fdefinitions ((cl-sf/bulk::%match-by-content
                           (lambda (&rest args) (declare (ignore args)) (error "boom"))))
        (handler-bind ((warning (lambda (w) (setf warned t) (muffle-warning w))))
          (let* ((result (cl-sf/bulk::%build-bulk-result nil "J" (list sent) :insert "Widget"
                                                         :upload (upload-of (list sent) columns) :columns columns))
                 (fresh (first (cl-sf/bulk:bulk-result-successful result))))
            (true warned "the failure is reported")
            (is string= "001K" (cl-sf/objects:sobject-id fresh) "...but the new Id is not lost")
            (false (eq fresh sent))))))))
