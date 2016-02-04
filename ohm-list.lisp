;;; ohm-list.lisp

(in-package #:cl-ohm)

(defclass ohm-list ()
  ((key :reader list-key
        :initarg :key)
   (element-type :reader element-type
                 :initarg :element-type)))

(defgeneric list-add (list element)
  (:documentation "Adds a ELEMENT to the LIST. ELEMENT must be a persistable object.")
  (:method :before ((list ohm-list) (element ohm-object))
           (ensure-id element))
  (:method ((list ohm-list) (element ohm-object))
    (with-connection ()
      (red:rpush (list-key list) (ohm-id element)))))

(defgeneric list-add-left (list element)
  (:documentation "Add ELEMENT to the left side of LIST.")
  (:method :before ((list ohm-list) (element ohm-object))
           (declare (ignore list))
           (ensure-id element))
  (:method ((list ohm-list) (element ohm-object))
    (with-connection ()
      (red:lpush (list-key list) (ohm-id element)))))

(defgeneric list-size (list)
  (:documentation "Returns the number of elements in the LIST.")
  (:method ((list ohm-list))
    (with-connection ()
      (red:llen (list-key list)))))

(defgeneric list-remove (list element)
  (:documentation "Removes ELEMENT from the LIST.")
  (:method :before ((list ohm-list) (element ohm-object))
           (declare (ignore list))
           (ensure-id element))
  (:method ((list ohm-list) (element ohm-object))
    (with-connection ()
      (red:lrem (list-key list) 0 (ohm-id element)))))

(defun list-access (list pop-func &rest args)
  "Returns an object if pop-func delivers an id."
  (let ((id (with-connection ()
              (apply pop-func (list-key list) args))))
    (when id
      (plist->object (element-type list)
                     (fetch-one (element-type list) id)))))

(defgeneric list-pop (list)
  (:documentation "Removes and returns an the right outermost element from the LIST.")
  (:method ((list ohm-list))
    (list-access list #'red:rpop)))

(defgeneric list-pop-left (list)
  (:documentation "Removes and returns the left outermost element of the LIST.")
  (:method ((list ohm-list))
    (list-access list #'red:lpop)))

(defgeneric list-index (list index)
  (:documentation "Returns the element with INDEX from LIST.")
  (:method ((list ohm-list) (index integer))
    (list-access list #'red:lindex index)))

(defgeneric list-first (list)
  (:documentation "Returns the first element of the LIST.")
  (:method ((list ohm-list))
    (list-index list 0)))

(defgeneric list-last (list)
  (:documentation "Returns the last element of the LIST.")
  (:method ((list ohm-list))
    (list-index list -1)))

(defgeneric list-ids (list &optional start stop)
  (:documentation "Returns the IDs stored in LIST.")
  (:method ((list ohm-list) &optional (start 0) (stop -1))
    (with-connection ()
      (red:lrange (list-key list) start stop))))

(defgeneric list-member (list element)
  (:documentation "Checks if ELEMENT is a member of LIST.")
  (:method :before ((list ohm-list) (element ohm-object))
           (declare (ignore list))
           (ensure-id element))
  (:method ((list ohm-list) (element ohm-object))
    (member (ohm-id element)
            (list-ids list)
            :test #'string=)))

(defgeneric list-range (list start stop)
  (:documentation "Returns a sublist of LIST from START to STOP.")
  (:method ((list ohm-list) (start integer) (stop integer))
    (mapcar (lambda (plist)
              (plist->object (element-type list) plist))
            (fetch (element-type list)
                   (list-ids list)))))

(defgeneric list-replace (list new-elements)
  (:documentation "Replaces the LIST elements with NEW-ELEMENTS.")
  (:method :before ((list ohm-list) new-elements)
           (declare (ignore list))
           (mapc #'ensure-id new-elements))
  (:method ((list ohm-list) new-elements)
    (let ((ids (mapcar #'ohm-id new-elements)))
      (with-connection ()
        (with-transaction
          (red:del (list-key list))
          (dolist (id ids)
            (red:rpush (list-key list) id)))))))

(defgeneric list-elements (list)
  (:documentation "Returns all elements in LIST.")
  (:method ((list ohm-list))
    (list-range list 0 -1)))
