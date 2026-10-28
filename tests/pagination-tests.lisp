;;;; cl-sf/tests/pagination-tests.lisp
;;;; Unit tests for cl-sf/pagination — no HTTP calls.
;;;; All tests use done=T responses so no lparallel future is fired
;;;; and conn=nil is safe.

(in-package #:cl-sf/tests)

(define-test pagination
  :parent cl-sf/tests
  :description "Pagination — lazy result set construction and traversal.")

;;; ──────────────────────────────────────────────────────────────────
;;; Helpers
;;; ──────────────────────────────────────────────────────────────────

(defun make-record-vector (&rest ids)
  "Build a vector of simple record hash-tables, each with an Id."
  (coerce (mapcar (lambda (id)
                    (make-hash "Id" id "Name" (format nil "Record-~A" id)))
                  ids)
          'vector))

;;; ──────────────────────────────────────────────────────────────────
;;; make-lazy-result-set
;;; ──────────────────────────────────────────────────────────────────

(define-test make-lazy-result-set/basic
  :parent pagination
  (let* ((records (make-record-vector "001" "002" "003"))
         (rset    (cl-sf/pagination:make-lazy-result-set
                   nil
                   (mock-query-response :total 3 :done t :records records))))
    (is = 3 (cl-sf/pagination:lazy-result-set-total-size rset))
    (false (cl-sf/pagination::lazy-result-set-next-records-url rset)
           "No next page URL when done=T")))

(define-test make-lazy-result-set/empty
  :parent pagination
  (let* ((rset (cl-sf/pagination:make-lazy-result-set
                nil
                (mock-query-response :total 0 :done t :records (vector)))))
    (true (cl-sf/pagination:result-set-exhausted-p rset)
          "Empty result set is immediately exhausted")))

(define-test make-lazy-result-set/custom-coercer
  :parent pagination
  (let* ((records (make-record-vector "001"))
         (rset    (cl-sf/pagination:make-lazy-result-set
                   nil
                   (mock-query-response :total 1 :done t :records records)
                   :record-coercer (lambda (h) (gethash "Id" h)))))
    (is string= "001" (cl-sf/pagination:next-record rset)
        "Coercer transforms record to Id string")))

;;; ──────────────────────────────────────────────────────────────────
;;; result-set-exhausted-p
;;; ──────────────────────────────────────────────────────────────────

(define-test result-set-exhausted-p/false-with-records
  :parent pagination
  (let* ((rset (cl-sf/pagination:make-lazy-result-set
                nil
                (mock-query-response :total 2 :done t
                                     :records (make-record-vector "001" "002")))))
    (false (cl-sf/pagination:result-set-exhausted-p rset)
           "Non-exhausted result set")))

(define-test result-set-exhausted-p/true-when-empty
  :parent pagination
  (let* ((rset (cl-sf/pagination:make-lazy-result-set
                nil
                (mock-query-response :total 0 :done t :records (vector)))))
    (true (cl-sf/pagination:result-set-exhausted-p rset))))

;;; ──────────────────────────────────────────────────────────────────
;;; next-record
;;; ──────────────────────────────────────────────────────────────────

(define-test next-record/returns-in-order
  :parent pagination
  (let* ((records (make-record-vector "001" "002" "003"))
         (rset    (cl-sf/pagination:make-lazy-result-set
                   nil
                   (mock-query-response :total 3 :done t :records records))))
    (is string= "001" (gethash "Id" (cl-sf/pagination:next-record rset)))
    (is string= "002" (gethash "Id" (cl-sf/pagination:next-record rset)))
    (is string= "003" (gethash "Id" (cl-sf/pagination:next-record rset)))))

(define-test next-record/returns-nil-when-exhausted
  :parent pagination
  (let* ((records (make-record-vector "001"))
         (rset    (cl-sf/pagination:make-lazy-result-set
                   nil
                   (mock-query-response :total 1 :done t :records records))))
    (cl-sf/pagination:next-record rset)   ; consume the one record
    (false (cl-sf/pagination:next-record rset)
           "next-record returns NIL after last record")))

(define-test next-record/coercer-applied-to-every-record
  :parent pagination
  (let* ((records (make-record-vector "001" "002"))
         (coerced '())
         (rset    (cl-sf/pagination:make-lazy-result-set
                   nil
                   (mock-query-response :total 2 :done t :records records)
                   :record-coercer (lambda (h)
                                     (let ((id (gethash "Id" h)))
                                       (push id coerced)
                                       id)))))
    (cl-sf/pagination:next-record rset)
    (cl-sf/pagination:next-record rset)
    (is equal '("002" "001") coerced
        "Coercer was called for each record in order")))

(define-test next-record/exhausted-after-drain
  :parent pagination
  (let* ((records (make-record-vector "001" "002"))
         (rset    (cl-sf/pagination:make-lazy-result-set
                   nil
                   (mock-query-response :total 2 :done t :records records))))
    (cl-sf/pagination:next-record rset)
    (cl-sf/pagination:next-record rset)
    (true (cl-sf/pagination:result-set-exhausted-p rset))))

;;; ──────────────────────────────────────────────────────────────────
;;; collect-all
;;; ──────────────────────────────────────────────────────────────────

(define-test collect-all/returns-all-records
  :parent pagination
  (let* ((records (make-record-vector "001" "002" "003"))
         (rset    (cl-sf/pagination:make-lazy-result-set
                   nil
                   (mock-query-response :total 3 :done t :records records)))
         (result  (cl-sf/pagination:collect-all rset)))
    (is = 3 (length result))
    (is string= "001" (gethash "Id" (first result)))
    (is string= "003" (gethash "Id" (third result)))))

(define-test collect-all/empty-result-set
  :parent pagination
  (let* ((rset   (cl-sf/pagination:make-lazy-result-set
                  nil
                  (mock-query-response :total 0 :done t :records (vector))))
         (result (cl-sf/pagination:collect-all rset)))
    (false result "collect-all on empty set returns NIL")))

(define-test collect-all/with-coercer
  :parent pagination
  (let* ((records (make-record-vector "001" "002"))
         (rset    (cl-sf/pagination:make-lazy-result-set
                   nil
                   (mock-query-response :total 2 :done t :records records)
                   :record-coercer (lambda (h) (gethash "Id" h))))
         (result  (cl-sf/pagination:collect-all rset)))
    (is equal '("001" "002") result
        "collect-all with Id-extracting coercer returns list of Id strings")))
