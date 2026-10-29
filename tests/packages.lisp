;;;; cl-sf/tests/packages.lisp
;;;; Test package, root suite, and shared helpers.

(defpackage #:cl-sf/tests
  (:use #:cl #:parachute)
  (:documentation "cl-sf test suite.  Run with (parachute:test 'cl-sf/tests)."))

(in-package #:cl-sf/tests)

;;; ──────────────────────────────────────────────────────────────────
;;; Root test suite
;;; All child suites use :parent cl-sf/tests
;;; ──────────────────────────────────────────────────────────────────

(define-test cl-sf/tests
  :description "cl-sf Salesforce client library — full test suite")

;;; ──────────────────────────────────────────────────────────────────
;;; Helpers
;;; ──────────────────────────────────────────────────────────────────

(defun make-hash (&rest pairs)
  "Create an EQUAL hash-table from alternating key-value PAIRS.
   (make-hash \"a\" 1 \"b\" 2) → {\"a\"→1, \"b\"→2}"
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on pairs by #'cddr
          do (setf (gethash k h) v))
    h))

(defun csv-text (octets)
  "UTF-8 OCTETS as a string."
  (sb-ext:octets-to-string octets :external-format :utf-8))

(defun csv-field (value)
  "Render VALUE as one CSV cell, as a string (via %write-field)."
  (let ((buf (cl-sf/bulk::%make-obuf 4)))
    (cl-sf/bulk::%write-field buf value)
    (csv-text (cl-sf/bulk::%obuf-octets buf))))

(defun mock-query-response (&key (total 0) (done t) records next-url)
  "Build a jzon-style hash-table mimicking a Salesforce /query response."
  (let ((h (make-hash-table :test #'equal)))
    (setf (gethash "totalSize"      h) total
          (gethash "done"           h) done
          (gethash "records"        h) (or records (make-array 0))
          (gethash "nextRecordsUrl" h) next-url)
    h))

(defun make-temp-dir ()
  "Create and return a fresh temp directory pathname (with trailing slash)."
  (let ((dir (merge-pathnames (format nil "cl-sf-test-~A/" (random most-positive-fixnum))
                              (uiop:temporary-directory))))
    (ensure-directories-exist dir)
    dir))

(defun write-fixture-json (dir relative-path data)
  "Write DATA (a hash-table) as JSON to DIR/RELATIVE-PATH, creating parent dirs."
  (let ((path (merge-pathnames relative-path dir)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (write-string (com.inuoe.jzon:stringify data) out))))

(defun delete-temp-dir (dir)
  "Best-effort recursive delete of a temp dir created by MAKE-TEMP-DIR."
  (ignore-errors (uiop:delete-directory-tree dir :validate t)))

(defun mock-record (&rest field-pairs)
  "Build a jzon-style record hash-table with an attributes block.
   (mock-record \"Id\" \"001\" \"Name\" \"Acme\")"
  (let ((h (make-hash-table :test #'equal))
        (attrs (make-hash-table :test #'equal)))
    (setf (gethash "type" attrs) "Account"
          (gethash "attributes" h) attrs)
    (loop for (k v) on field-pairs by #'cddr
          do (setf (gethash k h) v))
    h))
