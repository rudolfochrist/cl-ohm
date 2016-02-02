;;; ohm-object.lisp

(in-package #:CL-OHM)

(defclass ohm-object ()
  ((id :reader ohm-id)))

(defmethod print-object ((object ohm-object) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (loop for (slot . rest) on (closer-mop:class-slots (class-of object))
       for slot-name = (closer-mop:slot-definition-name slot)
       do
         (when (slot-boundp object slot-name)
           (format stream "~A=~A"
                   slot-name
                   (slot-value object slot-name))
           (when rest
             (format stream " "))))))

(defmacro define-ohm-model (name superclasses &key attributes)
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
                attributes))
     (:metaclass ohm-class)))

(defun object->plist (object)
  "Creates a plist of OBJECT's attributes."
  (let ((attributes (closer-mop:class-slots (class-of object))))
    (loop for attribute in attributes
       nconc
         (let ((attribute-name (closer-mop:slot-definition-name attribute)))
           (when (slot-boundp object attribute-name)
             (list (make-keyword attribute-name)
                   (slot-value object attribute-name)))))))
