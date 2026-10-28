(push #p"C:/Users/bscur/quicklisp/local-projects/cl-sf/" asdf:*central-registry*)
(asdf:load-system :cl-sf)
(in-package :cl-sf/objects)

;;; Test: define-sobject-class with special characters in field names
(format t "~&Test: define-sobject-class with field name handling~%")
(define-sobject-class custom-obj "Custom__c"
  (my-field "My_Field__c" :type (or string null))
  (another-field "Another__c" :type (or string null)))

(let ((obj (make-instance 'custom-obj)))
  (setf (custom-obj-my-field obj) "test")
  (format t "Field value: ~S~%" (custom-obj-my-field obj))
  (let ((serialized (sobject->hash-table obj)))
    (format t "Serialized: ~S~%" serialized)))

;;; Test: sobject-field-names
(format t "~&Test: sobject-field-names~%")
(let ((obj (make-instance 'custom-obj)))
  (let ((fields (sobject-field-names obj)))
    (format t "Fields: ~{~A~^ ~}~%" fields)))

;;; Test: compare slot-map registration
(format t "~&Test: Check slot-map has correct entries~%")
(let ((slot-map (gethash 'custom-obj *sobject-slot-maps*)))
  (format t "Slot map: ~S~%" slot-map)
  (let ((my-entry (assoc "My_Field__c" slot-map :test #'equal)))
    (format t "My_Field__c entry: ~S~%" my-entry)))

(format t "~&Done~%")
(sb-ext:exit)
