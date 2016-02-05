;;; ohm-set.lisp

(in-package #:cl-ohm)

(defclass ohm-set ()
  ;; TODO: those slots should be refactored to it's own
  ;; mixin, since it's basically the same everywhere.
  ((key :reader set-key
        :initarg :key)
   (element-type :reader element-type
                 :initarg :element-type)))

(defgeneric set-add (set element)
  (:documentation "Add the ELEMENT to the SET. Except ELEMENT is already a member.")
  (:method :before ((set ohm-set) (element ohm-object))
           (declare (ignore set))
           (ensure-id element))
  (:method ((set ohm-set) (element ohm-object))
    (with-connection ()
      (red:sadd (set-key set) (ohm-id element)))))

(defgeneric set-remove (set element)
  (:documentation "Removes the ELEMENT from the SET. Does nothing if ELEMENT is not a member.")
  (:method :before ((set ohm-set) (element ohm-object))
           (declare (ignore set))
           (ensure-id element))
  (:method ((set ohm-set) (element ohm-object))
    (with-connection ()
      (red:srem (set-key set) (ohm-id element)))))

(defgeneric set-replace (set new-elements)
  (:documentation "Replaces all elements in SET with NEW-ELEMENTS.")
  (:method :before ((set ohm-set) new-elements)
           (declare (ignore set))
           (mapc #'ensure-id new-elements))
  (:method ((set ohm-set) new-elements)
    (let ((ids (mapcar #'ohm-id new-elements)))
      (with-connection ()
        (with-transaction
          (red:del (set-key set))
          (apply #'red:sadd (set-key set) ids))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  SET OPERATIONS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric set-size (set)
  (:documentation "Returns the number of elements in the SET.")
  (:method ((set ohm-set))
    (execute (list 'red:scard (set-key set)))))

(defgeneric set-find-id (set id)
  (:documentation "Checks if ID is a member of SET.")
  (:method ((set ohm-set) (id integer))
    (set-find-id set (prin1-to-string id)))
  (:method ((set ohm-set) (id string))
    (execute (list 'red:sismember (set-key set) id))))

(defgeneric set-member (set element)
  (:documentation "Checks if ELEMENT is a member of SET.")
  (:method :before ((set ohm-set) (element ohm-object))
           (declare (ignore set))
           (ensure-id element))
  (:method ((set ohm-set) (element ohm-object))
    (set-find-id set (ohm-id element))))

(defgeneric set-ids (set)
  (:documentation "Returns the IDs contained in SET.")
  (:method ((set ohm-set))
    (etypecase (set-key set)
      (list
       (execute (set-key set)))
      (t
       (with-connection ()
         (red:smembers (set-key set)))))))

(defgeneric set-elements (set)
  (:documentation "Return the elements of SET.")
  (:method ((set ohm-set))
    (let ((ids (set-ids set)))
      (mapcar (lambda (element)
                (plist->object (element-type set) element))
              (fetch (element-type set) ids)))))

(defmacro assert-same-type (type1 type2)
  (once-only (type1 type2)
    `(assert (eql ,type1 ,type2)
             nil
             "Cannot union sets with different element types ~A and ~A"
             ,type1 ,type2)))

(defgeneric set-union (set1 set2)
  (:documentation "Union two sets. ARGS are used for FILTER.")
  (:method ((set1 ohm-set) (set2 ohm-set))
    (assert-same-type (element-type set1)
                      (element-type set2))
    (make-instance 'ohm-set
                   :element-type (element-type set1)
                   :key (list 'red:sunion
                              (set-key set1)
                              (set-key set2)))))

(defgeneric set-combine (set1 set2)
  (:documentation "Combines SET1 and SET2. Some would say intersection.")
  (:method ((set1 ohm-set) (set2 ohm-set))
    (assert-same-type (element-type set1)
                      (element-type set2))
    (make-instance 'ohm-set
                   :element-type (element-type set1)
                   :key (list 'red:sinter
                              (set-key set1)
                              (set-key set2)))))

(defgeneric set-except (set1 set2)
  (:documentation "Removes elements from SET1 that are also in SET2. Same people would say set-difference.")
  (:method ((set1 ohm-set) (set2 ohm-set))
    (assert-same-type (element-type set1)
                      (element-type set2))
    (make-instance 'ohm-set
                   :element-type (element-type set1)
                   :key (list 'red:sdiff
                              (set-key set1)
                              (set-key set2)))))
