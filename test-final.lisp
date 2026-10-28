;; Quick test without ASDF to avoid compilation issues
(defun show-hash (h)
  (maphash (lambda (k v) (format t "    ~S => ~S~%" k v)) h))

;;; Inline copy of what's needed from pagination.lisp
(defclass lazy-result-set () ())

;;; Test 1: SOQL compilation issue with (list ...)
(format t "~&=== BUG DEMONSTRATION ===~%")
(format t "~&BUG 1: SOQL with (list ...) in IN clause~%")
(format t "Code: (soql SELECT Id FROM Account WHERE (IN Status (list 'A' 'B')))~%")
(format t "Should generate: \"SELECT ID FROM ACCOUNT WHERE STATUS IN ('A', 'B')\"~%")
(format t "This WOULD show the bug if we could load the full system~%~%")

;;; Test 2: Bound variable issue
(format t "BUG 2: SOQL with bound variable in IN clause~%")
(format t "Code: (let ((statuses (list 'A' 'B')))~%")
(format t "         (soql SELECT Id FROM Account WHERE (IN Status statuses)))~%")
(format t "Error: 'cl-sf: malformed SOQL value list: STATUSES'~%")
(format t "This WOULD occur if we could load the full system~%~%")

(format t "These bugs are in the SOQL macro compilation (lines 376-384).~%")
(format t "Done~%")
(quit)
