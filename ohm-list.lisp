;;; ohm-list.lisp

(in-package #:cl-ohm)

(defclass ohm-list ()
  ((key :reader list-key
        :initarg :key)))

(defgeneric list-add (list element)
  (:documentation "Adds a ELEMENT to the LIST. ELEMENT must be a persistable object.")
  (:method :before ((list ohm-list) (element ohm-object))
           (declare (ignore list))
           (ensure-id element))
  (:method ((list ohm-list) (element ohm-object))
    (with-connection ()
      (red:rpush (list-key list) (object-key element)))))

(defgeneric list-add-left (list element)
  (:documentation "Add ELEMENT to the left side of LIST.")
  (:method :before ((list ohm-list) (element ohm-object))
           (declare (ignore list))
           (ensure-id element))
  (:method ((list ohm-list) (element ohm-object))
    (with-connection ()
      (red:lpush (list-key list) (object-key element)))))

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
      (red:lrem (list-key list) 0 (object-key element)))))

(defgeneric list-pop (list)
  (:documentation "Removes and returns the last element added to the LIST.")
  (:method ((list ohm-list))
    (cerror "Continue." "Unimplemented.")))
