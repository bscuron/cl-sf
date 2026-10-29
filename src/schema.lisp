;;;; cl-sf/src/schema.lisp
;;;; The org's describe metadata, cached per org and sObject type: field
;;;; types and parent relationships.  Used by SELECT to render LOCAL-TIME
;;;; timestamps as date, time or datetime literals.

(in-package #:cl-sf/objects)

(defstruct (schema-type (:conc-name sct-))
  (fields  (make-hash-table :test #'equal))    ; UPCASE → API name
  (types   (make-hash-table :test #'equal))    ; UPCASE → describe type ("datetime", ...)
  (parents (make-hash-table :test #'equal)))   ; UPCASE → (relationship . referenceTo list)

(defvar *schema-types* (make-hash-table :test #'equal :synchronized t)
  "(instance-url . UPCASE-NAME) → SCHEMA-TYPE.")

(defun %schema-type (conn object)
  "The metadata for sObject OBJECT (a symbol or string, any case), cached per org."
  (let* ((name (etypecase object (string object) (symbol (symbol-name object))))
         (key  (cons (connection-instance-url conn) (string-upcase name))))
    (or (gethash key *schema-types*)
        (setf (gethash key *schema-types*)
              (let ((desc (describe-sobject name :conn conn))
                    (type (make-schema-type)))
                (loop for field across (gethash "fields" desc #())
                      do (let ((n   (gethash "name" field))
                               (rel (gethash "relationshipName" field)))
                           (setf (gethash (string-upcase n) (sct-fields type)) n
                                 (gethash (string-upcase n) (sct-types type)) (gethash "type" field))
                           (when (stringp rel)
                             (setf (gethash (string-upcase rel) (sct-parents type))
                                   (cons rel (coerce (gethash "referenceTo" field #()) 'list))))))
                type)))))

(defun %field-type (conn object path)
  "The describe type (\"date\", \"datetime\", \"string\", ...) of the field PATH
   on sObject OBJECT: \"CreatedDate\" or a dotted relationship path such as
   \"Owner.CreatedDate\".  NIL if it can't be worked out."
  (let ((type  (%schema-type conn object))
        (parts (uiop:split-string path :separator ".")))
    (loop for (part . more) on parts
          for u = (string-upcase part)
          do (let ((parent (gethash u (sct-parents type))))
               (cond (more (if (and parent (null (cddr parent)) (cdr parent))
                               (setf type (%schema-type conn (cadr parent)))
                               (return nil)))
                     (t (return (gethash u (sct-types type)))))))))

(defun %soql-render-value-for (value from field)
  "Render the runtime VALUE of a SELECT condition on FIELD of sObject FROM
   (strings, as the macro saw them).  Like %SOQL-RENDER-VALUE, except that a
   LOCAL-TIME:TIMESTAMP — or a list holding some — becomes a date, time or
   datetime literal according to the field's type in the org's metadata.
   Without a connection, or if the type can't be worked out, it is a datetime."
  (flet ((kind ()
           (or (ignore-errors
                (and from field *connection* (%field-type *connection* from field)))
               "datetime"))
         (timestamp-p (x) (typep x 'local-time:timestamp)))
    (cond ((timestamp-p value)
           (%timestamp-text value (kind)))
          ((and (typep value 'sequence) (not (stringp value)) (some #'timestamp-p value))
           (let ((kind (kind)))
             (format nil "(~{~A~^, ~})"
                     (map 'list (lambda (x) (if (timestamp-p x)
                                                (%timestamp-text x kind)
                                                (%soql-render-value x)))
                          value))))
          (t (%soql-render-value value)))))
