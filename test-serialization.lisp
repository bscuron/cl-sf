(push #p"C:/Users/bscur/quicklisp/local-projects/cl-sf/" asdf:*central-registry*)
(asdf:load-system :cl-sf)
(in-package :cl-sf/objects)

(defun show-hash (h label)
  (format t "~&~A:~%" label)
  (if h
      (maphash (lambda (k v) (format t "  ~S => ~S~%" k v)) h)
      (format t "  (nil)~%")))

;;; Test: sobject->hash-table includes all fields
(format t "~&Test: sobject->hash-table field inclusion~%")
(define-sobject-class test-obj "TestObj"
  (field-a "FieldA" :type (or string null))
  (field-b "FieldB" :type (or string null))
  (field-c "FieldC" :type (or string null)))

(let ((obj (make-instance 'test-obj :id "001" :type "TestObj")))
  (setf (test-obj-field-a obj) "Value A")
  (setf (test-obj-field-b obj) "Value B")
  ;; field-c is not set, should remain nil
  (let ((result (sobject->hash-table obj)))
    (show-hash result "Serialized (should have FieldA and FieldB, not FieldC)")))

;;; Test: sobject->hash-table excludes read-only fields
(format t "~&Test: Read-only field filtering~%")
(let ((hash (make-hash-table :test #'equal)))
  (setf (gethash "Id" hash) "001")
  (setf (gethash "Name" hash) "Test")
  (setf (gethash "CreatedDate" hash) "2024-01-01")
  (setf (gethash "CreatedById" hash) "005xx")
  (setf (gethash "attributes" hash) (make-hash-table :test #'equal))
  (setf (gethash "type" (gethash "attributes" hash)) "TestObj")
  
  (let ((obj (hash->sobject hash)))
    (let ((result (sobject->hash-table obj)))
      (show-hash result "Serialized (should NOT have Id, CreatedDate, CreatedById)"))))

(format t "~&Done~%")
(sb-ext:exit)
