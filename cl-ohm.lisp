;;; cl-ohm.lisp

(in-package #:cl-ohm)

(defvar *global-object-counter*
  'cl-ohm-global-object-counter)

(defvar *class-indices* (make-hash-table)
  "Mapping of which class defined which indices.")

(defun create (name &rest initargs)
  "Creates a persisted instance of NAME."
  (assert (subtypep name 'ohm-object)
          (name)
          "~A is not persistable. Please use DEFINE-OHM-MODEL."
          name)
  (with-connection ()
    (let ((id (prin1-to-string
               (red:incr *global-object-counter*))))
      (save (apply #'make-instance name :id id initargs)))))

(defun ensure-id (object)
  (unless (slot-boundp object 'id)
    (error 'ohm-missing-id-error)))

(defun make-key (&rest segments)
  (format nil "~{~A~^:~}" segments))

(defgeneric class-key (object &rest segments)
  (:documentation "Creates a persistence key for class of OBJECT.")
  (:method ((object ohm-object) &rest segments)
    (apply #'make-key (class-name (class-of object)) segments)))

(defgeneric object-key (object &rest segments)
  (:documentation "Creates a persistence key for OBJECT.")
  (:method :before ((object ohm-object) &rest segments)
           (declare (ignore segments))
           (ensure-id object))
  (:method ((object ohm-object) &rest segments)
    (apply #'make-key
           (class-key object)
           (ohm-id object)
           segments)))

(defun create-indices (object)
  "Create indices for attributes with :indexp t."
  (let ((class-name (class-name (class-of object))))
    (setf (gethash class-name *class-indices*) '())
    (loop for slot in (closer-mop:class-slots (class-of object))
       for slot-name = (closer-mop:slot-definition-name slot)
       when (and (slot-boundp object slot-name)
                 (indexp slot))
       do
         (let ((index-key (class-key object
                                     'indices
                                     slot-name
                                     (slot-value object slot-name))))
           (red:sadd index-key (ohm-id object))
           (red:sadd (object-key object 'indices) index-key)
           (pushnew (make-keyword slot-name)
                    (gethash class-name *class-indices*))))))

(defun check-uniques (object)
  "Checks for constraints on object properties."
  (let ((uniques (remove-if-not #'uniquep (closer-mop:class-slots (class-of object)))))
    (loop for slot in uniques
       for slot-name = (closer-mop:slot-definition-name slot)
       when (slot-boundp object slot-name)
       do (let ((key (class-key object 'uniques slot-name))
                (slot-value (slot-value object slot-name)))
            (when (red:hexists key slot-value)
              (error 'ohm-unique-constraint-violation
                     :value slot-value))
            (red:hset key slot-value (ohm-id object))))))

(defgeneric save (object)
  (:documentation "Saves an object into the data store.")
  (:method :before ((object ohm-object))
           (ensure-id object))
  (:method ((object ohm-object))
    (let ((plist (object->plist object)))
      (with-connection ()
        (check-uniques object)
        (with-pipelining
          (with-transaction
            (apply #'red:hmset (object-key object) plist)
            (red:sadd (class-key object 'all) (ohm-id object))
            (create-indices object))))
      object)))

(defun remove-uniques (object)
  (let ((uniques (remove-if-not
                  #'uniquep
                  (closer-mop:class-slots (class-of object)))))
    (loop for slot in uniques
       for slot-name = (closer-mop:slot-definition-name slot)
       when (slot-boundp object slot-name)
       do (let ((key (class-key object 'uniques slot-name)))
            (red:hdel key (slot-value object slot-name))))))

(defgeneric del (object)
  (:documentation "Removes OBJECT from the data store.")
  (:method :before ((object ohm-object))
           (ensure-id object))
  (:method ((object ohm-object))
    (with-connection ()
      (let ((indices (red:smembers (object-key object 'indices))))
        (with-transaction
          (remove-uniques object)
          (red:del (object-key object)
                   (object-key object 'indices)
                   (object-key object 'counters))
          (red:srem (class-key object 'all) (ohm-id object))
          (dolist (index indices)
            (red:srem index (ohm-id object))))))))

(defun map-indices (class-name attribute value)
  "Creates a list of index key for ATTRIBUTE and VALUE."
  (unless (cl:member attribute
                     (gethash class-name *class-indices*))
    (error 'ohm-no-index-found-error :attribute attribute))

  (typecase value
    (list
     (mapcar (lambda (item)
               (make-key class-name 'indices attribute item))
             value))
    (t
     (list (make-key class-name 'indices attribute value)))))

(defun map-attributes (class-name attributes)
  "Generates as list of index keys from ATTRIBUTES."
  (loop for (attr val) on attributes by #'cddr
     nconc (map-indices class-name attr val)))

(defun filter (class-name &rest kwargs)
  "Find objects in the data store.
If no keyword arguments are given, all objects of
CLASS-NAME fetched."
  (assert (subtypep class-name 'ohm-object)
          (class-name)
          "~A must be a persistable class."
          class-name)
  ;; create an unpersisted instance
  ;; to access the indices for this type.
  (let* ((keys (if (null kwargs)
                   (list (make-key class-name 'all))
                   (map-attributes class-name kwargs))))
    (if (> (length keys) 1)
        (make-instance 'ohm-set
                       :element-type class-name
                       :key (append (list 'red:sinter) keys))
        (make-instance 'ohm-set
                       :element-type class-name
                       :key (cl:first keys)))))

;;; TODO: put this into filter like:
;;; (filter 'person :id 5) or
;;; (filter 'person :id "5")
(defgeneric filter-id (class-name id)
  (:documentation "Retrieves an object by id from the data store.")
  (:method (class-name (id integer))
    (find-id class-name (prin1-to-string id)))
  (:method (class-name (id string))
    (let ((id-exists-p (with-connection ()
                         (red:sismember (make-key class-name 'all) id))))
      (when id-exists-p
        (plist->object class-name
                       (fetch-one class-name id))))))

;;; TODO: put that into filter like:
;;; (filter 'person :with (:email "e@example.org"))
(defun filter-with (class-name attribute value)
  "Find an object with unique value. e.g.
\(find-one 'user :email \"foo@foo.com\"\)"
  (let* ((key (make-key class-name 'uniques attribute))
         (id (with-connection ()
               (red:hget key value))))
    (when id
      (plist->object class-name
                     (fetch-one class-name id)))))
