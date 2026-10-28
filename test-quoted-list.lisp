(push #p"C:/Users/bscur/quicklisp/local-projects/cl-sf/" asdf:*central-registry*)
(asdf:load-system :cl-sf)
(in-package :cl-sf/objects)

;;; Test: IN with quoted list
(format t "~&Test: IN with (quote ...)~%")
(let ((q (soql SELECT Id FROM Account WHERE (IN Status (quote ("Active" "Pending"))))))
  (format t "Result: ~S~%" q))

(format t "~&Done~%")
(sb-ext:exit)
