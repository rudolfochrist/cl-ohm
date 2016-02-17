;;; ohm-list.lisp

(in-package #:cl-ohm)

(defclass ohm-list (ohm-collection)
  ())

(defmethod add ((list ohm-list) (element ohm-object))
  (with-connection ()
    (red:rpush (key list) (ohm-id element))))

(defgeneric add-left (list element)
  (:documentation "Add ELEMENT to the left side of LIST.")
  (:method :before ((list ohm-list) (element ohm-object))
           (declare (ignore list))
           (ensure-id element))
  (:method ((list ohm-list) (element ohm-object))
    (with-connection ()
      (red:lpush (key list) (ohm-id element)))))

(defmethod size ((list ohm-list))
  (with-connection ()
    (red:llen (key list))))

(defmethod remove ((list ohm-list) (element ohm-object))
  (with-connection ()
    (red:lrem (key list) 0 (ohm-id element))))

(defun access (list pop-func &rest args)
  "Returns an object if pop-func delivers an id."
  (let ((id (with-connection ()
              (apply pop-func (key list) args))))
    (when id
      (plist->object (element-type list)
                     (fetch-one (element-type list) id)))))

(defgeneric pop (list)
  (:documentation "Removes and returns an the right outermost element from the LIST.")
  (:method ((list ohm-list))
    (access list #'red:rpop)))

(defgeneric pop-left (list)
  (:documentation "Removes and returns the left outermost element of the LIST.")
  (:method ((list ohm-list))
    (access list #'red:lpop)))

(defgeneric index (list index)
  (:documentation "Returns the element with INDEX from LIST.")
  (:method ((list ohm-list) (index integer))
    (access list #'red:lindex index)))

(defgeneric first (list)
  (:documentation "Returns the first element of the LIST.")
  (:method ((list ohm-list))
    (index list 0)))

(defgeneric last (list)
  (:documentation "Returns the last element of the LIST.")
  (:method ((list ohm-list))
    (index list -1)))

(defgeneric list-ids (collection &optional start stop)
  (:documentation "Returns the IDs stored in COLLECTION.")
  (:method ((list ohm-list) &optional (start 0) (stop -1))
    (with-connection ()
      (red:lrange (key list) start stop))))

(defmethod member ((list ohm-list) (element ohm-object))
  (cl:member (ohm-id element)
             (list-ids list)
             :test #'string=))

(defgeneric range (list start stop)
  (:documentation "Returns a sublist of LIST from START to STOP.")
  (:method ((list ohm-list) (start integer) (stop integer))
    (mapcar (lambda (plist)
              (plist->object (element-type list) plist))
            (fetch (element-type list)
                   (list-ids list start stop)))))

(defmethod replace ((list ohm-list) new-elements)
  (let ((ids (mapcar #'ohm-id new-elements)))
    (with-connection ()
      (with-pipelining
        (with-transaction
          (red:del (key list))
          (dolist (id ids)
            (red:rpush (key list) id)))))))

(defmethod elements ((list ohm-list))
  (range list 0 -1))
