(push #p"C:/Users/bscur/quicklisp/local-projects/cl-sf/" asdf:*central-registry*)
(asdf:load-system :cl-sf)
(in-package :cl-sf/objects)

(defun show-hash (h label)
  (format t "~&~A:~%" label)
  (if h
      (maphash (lambda (k v) (format t "  ~S => ~S~%" k v)) h)
      (format t "  (nil)~%")))

;;; Test: hash->sobject preserves sf-raw for unknown types
(format t "~&Test: sf-raw preservation~%")
(let ((hash (make-hash-table :test #'equal)))
  (setf (gethash "Id" hash) "001")
  (setf (gethash "Name" hash) "Test")
  (setf (gethash "CustomField" hash) "Custom")
  
  (let ((obj (hash->sobject hash "UnknownType")))
    (format t "Type: ~S~%" (sobject-type obj))
    (format t "sf-raw is set: ~S~%" (not (null (sobject-raw obj))))
    (show-hash (sobject-raw obj) "sf-raw contents")))

;;; Test: sobject->hash-table on unknown type
(format t "~&Test: Serialization of unknown type~%")
(let ((hash (make-hash-table :test #'equal)))
  (setf (gethash "Id" hash) "001")
  (setf (gethash "Name" hash) "Test")
  (setf (gethash "CustomField" hash) "Custom")
  (setf (gethash "CreatedDate" hash) "2024-01-01")
  
  (let ((obj (hash->sobject hash "UnknownType")))
    (let ((result (sobject->hash-table obj)))
      (show-hash result "Serialized (should skip CreatedDate, keep others)"))))

(format t "~&Done~%")
(sb-ext:exit)
