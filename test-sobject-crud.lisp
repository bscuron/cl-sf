(push #p"C:/Users/bscur/quicklisp/local-projects/cl-sf/" asdf:*central-registry*)
(asdf:load-system :cl-sf)
(in-package :cl-sf/objects)

;;; Test sobject-create mock result handling
(format t "~&Test: sobject-create result handling~%")
(define-sobject-class test-account "TestAccount"
  (name "Name" :type (or string null)))

(let ((obj (make-instance 'test-account :name "Test Corp")))
  (format t "Before: sobject-raw = ~S~%" (sobject-raw obj))
  
  ;; Simulate the update from sobject-create
  (let ((result (make-hash-table :test #'equal)))
    (setf (gethash "id" result) "001xx000000001AAA")
    (setf (gethash "success" result) t)
    ;; In the actual code (line 628-630):
    (setf (sobject-id obj) (gethash "id" result))
    (setf (sobject-raw obj) (or (sobject-raw obj)
                                 (make-hash-table :test #'equal)))
    (format t "After create: sobject-raw = ~S~%" (sobject-raw obj))
    (format t "Created Id: ~S~%" (sobject-id obj))
    (format t "NOTE: sobject-raw is empty - no response data stored!~%")))

;;; Test sobject->hash-table with nil values
(format t "~&Test: sobject->hash-table nil value filtering~%")
(let ((hash (make-hash-table :test #'equal)))
  (setf (gethash "Id" hash) "001")
  (setf (gethash "Name" hash) "Acme")
  (setf (gethash "Phone" hash) nil)
  (setf (gethash "attributes" hash) (make-hash-table :test #'equal))
  (setf (gethash "type" (gethash "attributes" hash)) "TestAccount")
  
  (let ((obj (hash->sobject hash)))
    (let ((serialized (sobject->hash-table obj)))
      (format t "Original has Phone=nil~%")
      (format t "Serialized keys: ~{~A~^ ~}~%" (loop for k being the hash-keys of serialized collect k))
      (format t "Phone in output: ~S~%" (gethash "Phone" serialized)))))

;;; Test hash->sobject with empty type
(format t "~&Test: hash->sobject with no type~%")
(let ((hash (make-hash-table :test #'equal)))
  (setf (gethash "Id" hash) "001")
  (setf (gethash "Name" hash) "Unknown")
  (let ((obj (hash->sobject hash)))
    (format t "Type: ~S (empty string)~%" (sobject-type obj))
    (format t "Is SOBJECT: ~S~%" (typep obj 'sobject))))

(format t "~&Done~%")
(sb-ext:exit)
