;;; cl-ohm.lisp

(in-package #:cl-ohm)

(defvar *global-object-counter*
  'cl-ohm-global-object-counter)

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
  (:method :before ((object ohm-object) &rest segments)
           (declare (ignore segments))
           (ensure-id object))
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
         (red:sadd (object-key object 'indices) index-key))))

(defgeneric save (object)
  (:documentation "Saves an object into the data store.")
  (:method :before ((object ohm-object))
           (ensure-id object))
  (:method ((object ohm-object))
    (let ((plist (object->plist object)))
      (with-connection ()
        (with-transaction ()
          (apply #'red:hmset (object-key object) plist)
          (red:sadd (class-key object 'all) (ohm-id object))
          (create-indices object)))
      object)))

(defgeneric del (object)
  (:documentation "Removes OBJECT from the data store.")
  (:method :before ((object ohm-object))
           (ensure-id object))
  (:method ((object ohm-object))
    (with-connection ()
      (let ((indices (red:smembers (object-key object 'indices))))
        (with-transaction ()
          (red:del (object-key object)
                   (object-key object 'indices)
                   (object-key object 'counters))
          (red:srem (class-key object 'all) (ohm-id object))
          (dolist (index indices)
            (red:srem index (ohm-id object))))))))
