;;; ohm-counter.lisp

(in-package #:CL-OHM)

(defclass ohm-counter ()
  ((name :reader counter-name
         :initarg :name)
   (key :reader counter-key
        :initarg :key)))

(defgeneric counter (counter)
  (:documentation "Retrieves the value of the counter COUNTER/")
  (:method ((counter ohm-counter))
    (with-connection ()
      (let ((value (red:hget (counter-key counter)
                             (counter-name counter))))
        (when value
          (parse-integer value :junk-allowed t))))))

(defgeneric incr (counter &optional increment)
  (:documentation "Increments COUNTER by INCREMENT.")
  (:method ((counter ohm-counter) &optional (increment 1))
    (with-connection ()
      (red:hincrby (counter-key counter)
                   (counter-name counter)
                   increment))))

(defgeneric decr (counter &optional decrement)
  (:documentation "Decrements COUNTER by DECREMENT.")
  (:method ((counter ohm-counter) &optional (decrement 1))
    (incr counter (- decrement))))
