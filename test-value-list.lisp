(push #p"C:/Users/bscur/quicklisp/local-projects/cl-sf/" asdf:*central-registry*)
(asdf:load-system :cl-sf)
(in-package :cl-sf/objects)

;;; Test 1: IN with direct list literal
(format t "~&Test 1: IN with direct list~%")
(let ((q (soql SELECT Id FROM Account WHERE (IN Status ("Active" "Pending")))))
  (format t "Result: ~S~%" q))

;;; Test 2: IN with empty list
(format t "~&Test 2: IN with empty list~%")
(let ((q (soql SELECT Id FROM Account WHERE (IN Status ()))))
  (format t "Result: ~S~%" q))

;;; Test 3: IN with single element
(format t "~&Test 3: IN with single element~%")
(let ((q (soql SELECT Id FROM Account WHERE (IN Status ("Active")))))
  (format t "Result: ~S~%" q))

;;; Test 4: IN with numeric values
(format t "~&Test 4: IN with numbers~%")
(let ((q (soql SELECT Id FROM Account WHERE (IN Id (1 2 3)))))
  (format t "Result: ~S~%" q))

;;; Test 5: IN with mixed quoted/unquoted
(format t "~&Test 5: IN with mixed~%")
(let ((q (soql SELECT Id FROM Account WHERE (IN Status ("A" B 3)))))
  (format t "Result: ~S~%" q))

(format t "~&Done~%")
(sb-ext:exit)
