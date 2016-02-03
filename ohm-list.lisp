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

(defun generic-pop (list pop-func)
  "Returns an object if pop-func delivers an id."
  (let ((id (with-connection ()
              (funcall pop-func (list-key list)))))
    (when id
      (plist->object (element-type list)
                     (fetch-one (element-type list) id)))))

(defgeneric list-pop (list)
  (:documentation "Removes and returns an the right outermost element from the LIST.")
  (:method ((list ohm-list))
    (generic-pop list #'red:rpop)))

(defgeneric list-pop-left (list)
  (:documentation "Removes and returns the left outermost element of the LIST.")
  (:method ((list ohm-list))
    (generic-pop list #'red:lpop)))

(defgeneric list-index (list index)
  (:documentation "Returns the element with INDEX from LIST.")
  (:method ((list ohm-list) (index integer))
    (let ((id (with-connection ()
                (red:lindex (list-key list) index))))
      (when id
        (plist->object (element-type list)
                       (fetch-one (element-type list) id))))))

(defgeneric list-first (list)
  (:documentation "Returns the first element of the LIST.")
  (:method ((list ohm-list))
    (list-index list 0)))

(defgeneric list-last (list)
  (:documentation "Returns the last element of the LIST.")
  (:method ((list ohm-list))
    (list-index list -1)))

(defgeneric list-member (list element)
  (:documentation "Checks if ELEMENT is a member of LIST.")
  (:method :before ((list ohm-list) (element ohm-object))
           (declare (ignore list))
           (ensure-id element))
  (:method ((list ohm-list) (element ohm-object))
    (let ((ids (with-connection ()
                 (red:lrange (list-key list) 0 -1)))
          (element-id (ohm-id element)))
      (member element-id ids :test #'string=))))
