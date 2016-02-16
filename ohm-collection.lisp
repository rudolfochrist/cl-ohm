;;; ohm-collection.lisp

(defclass ohm-collection ()
  ((key :reader key
        :initarg :key)
   (element-type :reader element-type
                 :initarg :element-type)))


;;;;;;;;;;;;;;;;;;;;;;;;;;  OHM-COLLECTION PROTOCOL  ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric add (collection element)
  (:documentation "Adds a ELEMENT to the COLLECTION. ELEMENT must be a persistable object.")
  (:method :before ((collection ohm-collection) (element ohm-object))
           (declare (ignore collection))
           (ensure-id element)))

(defgeneric remove (collection element)
  (:documentation "Removes ELEMENT from the COLLECTION.")
  (:method :before ((collection ohm-collection) (element ohm-object))
           (declare (ignore collection))
           (ensure-id element)))

(defgeneric replace (collection new-elements)
  (:documentation "Replaces the COLLECTION's elements with NEW-ELEMENTS.")
  (:method :before ((collection ohm-collection) new-elements)
           (declare (ignore collection))
           (mapc #'ensure-id new-elements)))

(defgeneric size (collection)
  (:documentation "Returns the number of elements in the COLLECTION."))

(defgeneric member (collection element)
  (:documentation "Checks if ELEMENT is a member of COLLECTION.")
  (:method :before ((collection ohm-collection) (element ohm-object))
           (declare (ignore collection))
           (ensure-id element)))

(defgeneric elements (collection)
  (:documentation "Returns all elements in COLLECTION."))
