(push #p"C:/Users/bscur/quicklisp/local-projects/cl-sf/" asdf:*central-registry*)
(asdf:load-system :cl-sf)
(in-package :cl-sf/objects)

;;; Test 1: IN with (val ...) escape
(format t "~&Test 1: IN with (val ...) escape~%")
(let ((q (soql SELECT Id FROM Account WHERE (IN Status (val (list "Active" "Pending"))))))
  (format t "Result: ~S~%" q))

;;; Test 2: IN with variable (if bound)
(format t "~&Test 2: IN with bound variable~%")
(let ((statuses (list "Active" "Pending")))
  (let ((q (soql SELECT Id FROM Account WHERE (IN Status statuses))))
    (format t "Result: ~S~%" q)))

;;; Test 3: IN with (val ...) and single quotes
(format t "~&Test 3: IN with val and string containing quotes~%")
(let ((q (soql SELECT Id FROM Account WHERE (IN Name (val (list "O'Brien" "Smith"))))))
  (format t "Result: ~S~%" q))

(format t "~&Done~%")
(sb-ext:exit)
