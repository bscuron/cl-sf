;;;; cl-sf/src/schema.lisp
;;;; The org's describe metadata, cached per org and sObject type: names in
;;;; any case (the Lisp reader upcases symbols; GraphQL is case-sensitive),
;;;; relationships, and field types.  Used by GQL to spell names and to wrap
;;;; date/datetime operands, and by SELECT to render LOCAL-TIME timestamps as
;;;; date or datetime literals.

(in-package #:cl-sf/objects)

;;; ──────────────────────────────────────────────────────────────────
;;; Name resolution from describe metadata
;;; ──────────────────────────────────────────────────────────────────

(defstruct (gql-type (:conc-name gqt-))
  (name nil)
  (fields   (make-hash-table :test #'equal))    ; UPCASE → API name
  (types    (make-hash-table :test #'equal))    ; UPCASE → describe type ("datetime", ...)
  (parents  (make-hash-table :test #'equal))    ; UPCASE → (relationship . referenceTo list)
  (children (make-hash-table :test #'equal)))   ; UPCASE → (relationship . child sObject)

(defvar *gql-types* (make-hash-table :test #'equal :synchronized t)
  "(instance-url . UPCASE-NAME) → GQL-TYPE.")

(defun %gql-name (x)
  (etypecase x (string x) (symbol (symbol-name x))))

(defun %gql-error (control &rest args)
  "A mistake in a gql form, found when the query is built: SF-API-ERROR, like
   everything else that can go wrong running a query."
  (error 'sf-api-error :message (format nil "gql: ~?" control args)))

(defun %gql-build-type (conn name register-class)
  (let* ((desc (handler-case (describe-sobject name :conn conn)
                 (sf-api-error (e)
                   (error 'sf-api-error
                          :message (format nil "gql: cannot describe ~A: ~A"
                                           name (sf-api-error-message e))))))
         (type (make-gql-type :name (gethash "name" desc))))
    (loop for field across (gethash "fields" desc #())
          do (let ((n   (gethash "name" field))
                   (rel (gethash "relationshipName" field)))
               (setf (gethash (string-upcase n) (gqt-fields type)) n
                     (gethash (string-upcase n) (gqt-types type)) (gethash "type" field))
               (when (stringp rel)
                 (setf (gethash (string-upcase rel) (gqt-parents type))
                       (cons rel (coerce (gethash "referenceTo" field #()) 'list))))))
    (loop for child across (gethash "childRelationships" desc #())
          do (let ((rel (gethash "relationshipName" child)))
               (when (stringp rel)
                 (setf (gethash (string-upcase rel) (gqt-children type))
                       (cons rel (gethash "childSObject" child))))))
    ;; the records come back as this class: reuse the describe we already have
    (when (and register-class (not (gethash (gqt-name type) *sobject-classes*)))
      (%register-sobject-class conn (gqt-name type) desc))
    type))

(defun %gql-type (conn object &key register-class)
  "The metadata for sObject OBJECT (a symbol or string, any case), cached per org."
  (let ((key (cons (connection-instance-url conn) (string-upcase (%gql-name object)))))
    (or (gethash key *gql-types*)
        (setf (gethash key *gql-types*)
              (%gql-build-type conn (%gql-name object) register-class)))))

(defparameter *gql-pseudo-fields*
  '(("__TYPENAME" . "__typename") ("APINAME" . "ApiName") ("WEAKETAG" . "WeakEtag"))
  "Selectable on any record but absent from describe.")

(defun %schema-lookup (type name)
  "NAME (a string, any case) resolved against TYPE → (values api-name kind data):
   KIND :scalar, :parent (DATA = referenceTo list) or :child (DATA = child
   sObject).  NIL if TYPE has no such field or relationship."
  (let* ((u (string-upcase name))
         (field  (gethash u (gqt-fields type)))
         (parent (gethash u (gqt-parents type)))
         (child  (gethash u (gqt-children type)))
         (pseudo (cdr (assoc u *gql-pseudo-fields* :test #'string=))))
    (cond (field  (values field :scalar nil))
          (parent (values (car parent) :parent (cdr parent)))
          (child  (values (car child) :child (cdr child)))
          (pseudo (values pseudo :scalar nil)))))

(defun %gql-lookup (type head)
  "Resolve HEAD against TYPE → (values api-name kind data), KIND one of
   :scalar :parent :child, or :verbatim (HEAD was a string, used as written).
   Signals SF-API-ERROR for a name TYPE doesn't have."
  (if (stringp head)
      (values head :verbatim nil)
      (multiple-value-bind (name kind data) (%schema-lookup type (symbol-name head))
        (if name
            (values name kind data)
            (error 'sf-api-error
                   :message (format nil "gql: ~A has no field or relationship named ~A"
                                    (gqt-name type) (symbol-name head)))))))

;;; — field types, for date/datetime values —

(defun %field-type (conn object path)
  "The describe type (\"date\", \"datetime\", \"string\", ...) of the field PATH
   on sObject OBJECT: \"CreatedDate\" or a dotted relationship path such as
   \"Owner.CreatedDate\".  NIL if it can't be worked out."
  (let ((type  (%gql-type conn object))
        (parts (uiop:split-string path :separator ".")))
    (loop for (part . more) on parts
          do (multiple-value-bind (name kind data) (%schema-lookup type part)
               (cond ((null name) (return nil))
                     (more (if (and (eq kind :parent) data (null (cdr data)))
                               (setf type (%gql-type conn (car data)))
                               (return nil)))
                     ((eq kind :scalar) (return (gethash (string-upcase name) (gqt-types type))))
                     (t (return nil)))))))

(defun %timestamp-text (timestamp kind)
  "TIMESTAMP (a LOCAL-TIME:TIMESTAMP, taken as UTC) as the literal for a field
   of describe type KIND: \"date\" → 2024-03-05, \"time\" → 03:04:05.000Z,
   anything else (datetime) → 2024-03-05T03:04:05Z."
  (local-time:format-timestring
   nil timestamp :timezone local-time:+utc-zone+
                 :format (cond ((equal kind "date")
                                '((:year 4) #\- (:month 2) #\- (:day 2)))
                               ((equal kind "time")
                                '((:hour 2) #\: (:min 2) #\: (:sec 2) #\. (:msec 3) #\Z))
                               (t
                                '((:year 4) #\- (:month 2) #\- (:day 2) #\T
                                  (:hour 2) #\: (:min 2) #\: (:sec 2) #\Z)))))

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
