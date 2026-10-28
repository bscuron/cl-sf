(push #p"C:/Users/bscur/quicklisp/local-projects/cl-sf/" asdf:*central-registry*)
(asdf:load-system :cl-sf)
(in-package :cl-sf/objects)

;;; Test SOQL WHERE conditions
(format t "~&Test 1: AND with multiple conditions~%")
(let ((q (soql SELECT Id FROM Account WHERE (AND (= Status "Active") (> Revenue 1000) (LIKE Name "Test%")))))
  (format t "Result: ~S~%" q))

;;; Test SOQL NOT condition
(format t "~&Test 2: NOT condition~%")
(let ((q (soql SELECT Id FROM Account WHERE (NOT (= Status "Closed")))))
  (format t "Result: ~S~%" q))

;;; Test SOQL OR condition
(format t "~&Test 3: OR condition~%")
(let ((q (soql SELECT Id FROM Account WHERE (OR (= Status "Active") (= Status "Pending")))))
  (format t "Result: ~S~%" q))

;;; Test ORDER BY with modifiers
(format t "~&Test 4: ORDER BY with ASC/DESC/NULLS~%")
(let ((q (soql SELECT Id Name FROM Account ORDER BY Name ASC CreatedDate DESC NULLS LAST)))
  (format t "Result: ~S~%" q))

;;; Test LIMIT/OFFSET
(format t "~&Test 5: LIMIT and OFFSET~%")
(let ((q (soql SELECT Id FROM Account LIMIT 10 OFFSET 5)))
  (format t "Result: ~S~%" q))

(format t "~&Done~%")
(sb-ext:exit)
