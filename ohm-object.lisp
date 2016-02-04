;;; ohm-object.lisp

(in-package #:CL-OHM)

(defclass ohm-object ()
  ((id :reader ohm-id
       :initarg :id)))

(defmethod print-object ((object ohm-object) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (loop for (slot . rest) on (closer-mop:class-slots (class-of object))
       for slot-name = (closer-mop:slot-definition-name slot)
       do
         (when (slot-boundp object slot-name)
           (format stream "~S=~S"
                   slot-name
                   (slot-value object slot-name))
           (when rest
             (format stream " "))))))

(defmacro define-ohm-model (name superclasses &key attributes counters lists sets)
  `(defclass ,name ,(if superclasses superclasses '(ohm-object))
     (,@(mapcar (lambda (attribute)
                  (unless (listp attribute)
                    (setf attribute (list attribute)))
                  (if (or (getf (cdr attribute) :reader)
                          (getf (cdr attribute) :writer)
                          (getf (cdr attribute) :accessor))
                      attribute
                      (append attribute
                              (list :accessor (car attribute)))))
                attributes)
      ,@(mapcar (lambda (counter)
                  `(,counter
                    :accessor ,counter
                    :counterp t))
                counters)
        ,@(mapcar (lambda (list)
                    (append list
                            (list :list-attr-p t
                                  :accessor (car list))))
                  lists)
        ,@(mapcar (lambda (set)
                    (append set
                            (list :set-attr-p t
                                  :accessor (car set))))
                  sets))
     (:metaclass ohm-class)))

(defmethod initialize-instance :after ((instance ohm-object) &key)
  (dolist (slot (closer-mop:class-slots (class-of instance)))
    (let ((slot-name (closer-mop:slot-definition-name slot)))

      ;; FIXME: this is quite ugly and needs refactoring. Keeping it for now.
      (cond
        ((counterp slot)
         (setf (slot-value instance slot-name)
               (make-instance 'ohm-counter
                              :key (object-key instance 'counters)
                              :name slot-name)))
        ((list-attr-p slot)
         (let ((element-type (element-type slot)))
           (assert (subtypep element-type 'ohm-object)
                   (element-type)
                   "Element type must be a persistable type. ~A is not persistable."
                   element-type)
           (setf (slot-value instance slot-name)
                 (make-instance 'ohm-list
                                :key (object-key instance slot-name)
                                :element-type element-type))))
        ((set-attr-p slot)
         (let ((element-type (element-type slot)))
           (assert (subtypep element-type 'ohm-object)
                   (element-type)
                   "Element type must be a persistable type. ~A is not persistable."
                   element-type)
           (setf (slot-value instance slot-name)
                 (make-instance 'ohm-set
                                :key (object-key instance slot-name)
                                :element-type element-type))))))))

(defun object->plist (object)
  "Creates a plist of OBJECT's attributes."
  (let ((attributes (remove-if (lambda (slot)
                                 (or (counterp slot)
                                     (list-attr-p slot)
                                     (set-attr-p slot)))
                               (closer-mop:class-slots (class-of object)))))
    (loop for attribute in attributes
       nconc
         (let ((attribute-name (closer-mop:slot-definition-name attribute)))
           (when (slot-boundp object attribute-name)
             (list (make-keyword attribute-name)
                   (slot-value object attribute-name)))))))

(defun normalize-plist (plist)
  "Creates a proper plist of the given TUPLE."
  (loop for (key value) on plist by #'cddr
     append (list (make-keyword key) value)))

(defun plist->object (class-name plist)
  "Creates an instance of CLASS-NAME with initargs found in plist."
  (let ((norm-plist (normalize-plist plist)))
    (apply #'make-instance class-name norm-plist)))

(defun fetch (namespace ids)
  "Loads objects from the data store."
  (with-connection ()
    (with-pipelining
      (loop for key in (keys namespace ids)
         do (red:hgetall key)))))

(defun fetch-one (namespace id)
  "Load one object from the data store."
  (first (fetch namespace (list id))))

(defun keys (namespace ids)
  "Makes a list of keys for the given NAMESPACE and IDS."
  (mapcar (lambda (id)
            (make-key namespace id))
          ids))
