;;; ohm-set.lisp

(in-package #:cl-ohm)

(defclass ohm-set (ohm-collection)
  ())

(defmethod add ((set ohm-set) (element ohm-object))
  (with-connection ()
    (red:sadd (key set) (ohm-id element))))

(defmethod remove ((set ohm-set) (element ohm-object))
  (with-connection ()
    (red:srem (key set) (ohm-id element))))

(defmethod replace ((set ohm-set) new-elements)
  (let ((ids (mapcar #'ohm-id new-elements)))
    (with-connection ()
      (with-pipelining
        (with-transaction
          (red:del (key set))
          (apply #'red:sadd (key set) ids))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  SET OPERATIONS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod size ((set ohm-set))
  (execute (list 'red:scard (key set))))

(defgeneric find-id (set id)
  (:documentation "Checks if ID is a member of SET.")
  (:method ((set ohm-set) (id integer))
    (find-id set (prin1-to-string id)))
  (:method ((set ohm-set) (id string))
    (execute (list 'red:sismember (key set) id))))

(defmethod member ((set ohm-set) (element ohm-object))
  (find-id set (ohm-id element)))

(defgeneric set-ids (set)
  (:documentation "Returns the IDs contained in SET.")
  (:method ((set ohm-set))
    (etypecase (key set)
      (list
       (execute (key set)))
      (t
       (with-connection ()
         (red:smembers (key set)))))))

(defmethod elements ((set ohm-set))
  (let ((ids (set-ids set)))
    (mapcar (lambda (element)
              (plist->object (element-type set) element))
            (fetch (element-type set) ids))))

(defmacro assert-same-type (type1 type2)
  (once-only (type1 type2)
    `(assert (eql ,type1 ,type2)
             nil
             "Cannot union sets with different element types ~A and ~A"
             ,type1 ,type2)))

(defun generic-set-operation (set1 set2 op)
  (assert-same-type (element-type set1)
                    (element-type set2))
  (make-instance 'ohm-set
                 :element-type (element-type set1)
                 :key (list op
                            (key set1)
                            (key set2))))

(defgeneric union (set1 set2)
  (:documentation "Union two sets. ARGS are used for FILTER.")
  (:method ((set1 ohm-set) (set2 ohm-set))
    (generic-set-operation set1 set2 'red:sunion)))

(defgeneric combine (set1 set2)
  (:documentation "Combines SET1 and SET2. Some would say intersection.")
  (:method ((set1 ohm-set) (set2 ohm-set))
    (generic-set-operation set1 set2 'red:sinter)))

(defgeneric except (set1 set2)
  (:documentation "Removes elements from SET1 that are also in SET2. Same people would say set-difference.")
  (:method ((set1 ohm-set) (set2 ohm-set))
    (generic-set-operation set1 set2 'red:sdiff)))

(defun make-sort-key (set attribute)
  "Creates a sort key for the element type of SET.
ATTRIBUTE can either be a keyword, symbol or string."
  (make-key (element-type set)
            (format nil "*->~A"
                    (string-upcase (string attribute)))))

(defgeneric sort (set &key desc alpha start end by get store)
  (:documentation "Sorts the SET.")
  (:method ((set ohm-set) &key desc alpha start end by get store)
    (let ((args (append (when desc
                          (list :desc t))
                        (when alpha
                          (list :alpha t))
                        (when start
                          (list :start start
                                :end end))
                        (when by
                          (list :by by))
                        (when get
                          (list :get (make-sort-key set get)))
                        (when store
                          (list :store store)))))
      (if get
          (execute (append (list 'red:sort (key set)) args))
          (fetch (element-type set)
                 (execute (append (list 'red:sort (key set)) args)))))))

(defgeneric sort-by (set key &key desc alpha start end get store)
  (:documentation "Sorts the objects in SET by it's property KEY.")
  (:method ((set ohm-set) key &key desc alpha start end get store)
    (sort set
          :by (make-sort-key set key)
          :desc desc :alpha alpha
          :start start :end end
          :get get :store store)))
