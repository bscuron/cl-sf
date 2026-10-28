(push #p"C:/Users/bscur/quicklisp/local-projects/cl-sf/" asdf:*central-registry*)
(asdf:load-system :cl-sf)
(in-package :cl-sf/objects)

;;; Test 1: IN with (val ...) escape
(format t "~&Test 1: IN with (val ...) escape~%")
(let ((q (soql SELECT Id FROM Account WHERE (IN Status (val (list "Active" "Pending"))))))
  (format t "Result: ~S~%" q))

(format t "~&Done~%")
(sb-ext:exit)
