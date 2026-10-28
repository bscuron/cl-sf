(push #p"C:/Users/bscur/quicklisp/local-projects/cl-sf/" asdf:*central-registry*)
(asdf:load-system :cl-sf)
(in-package :cl-sf/objects)

;;; BUG: IN with (list...) mishandles the form
(format t "~&BUG CONFIRMATION: IN with (list ...) form~%")
(let ((q (soql SELECT Id FROM Account WHERE (IN Status (list "A" "B")))))
  (format t "Output: ~S~%" q)
  (format t "Problem: 'list' symbol rendered as LIST literal in IN clause~%"))

;;; Test to see if line 409 NOT-IN handling works correctly
(format t "~&Test: NOT-IN operator~%")
(let ((q (soql SELECT Id FROM Account WHERE (NOT-IN Status ("A" "B")))))
  (format t "Output: ~S~%" q)
  (if (search "NOT IN" q)
      (format t "PASS: NOT IN rendered correctly~%")
      (format t "FAIL: NOT IN not found in query~%")))

;;; Test sobject-create handling of response
(format t "~&Test: sobject-create result (line 628-630)~%")
(define-sobject-class test-acc "TestAcc"
  (name "Name"))
(let ((obj (make-instance 'test-acc :name "Acme")))
  (let ((result (make-hash-table :test #'equal)))
    (setf (gethash "id" result) "001xx000000001AAA")
    (setf (gethash "success" result) t)
    ;; Simulate what sobject-create does
    (setf (sobject-id obj) (gethash "id" result))
    (setf (sobject-raw obj) (or (sobject-raw obj)
                                 (make-hash-table :test #'equal)))
    (format t "Result data from API: ~S~%" result)
    (format t "Stored in sf-raw: ~S (ISSUE: result data not preserved!)~%" (sobject-raw obj))))

(format t "~&Done~%")
(sb-ext:exit)
