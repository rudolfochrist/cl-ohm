;;; connection.lisp

(in-package #:CL-OHM)

(defvar *default-connection-parameters*
  (list :host (vector 127 0 0 1)
        :port 6379
        :auth nil))

(defparameter *connection-parameters* *default-connection-parameters*)

(defun setup-redis-connection (&key
                                 (host #(127 0 0 1))
                                 (port 6379)
                                 auth)
  (setf (getf *connection-parameters* :host) host
        (getf *connection-parameters* :port) port
        (getf *connection-parameters* :auth) auth)
  *connection-parameters*)

(defmacro with-connection (() &body body)
  `(redis:with-connection ,*connection-parameters*
     ,@body))

(defmacro with-transaction (() &body body)
  `(progn
     (red:multi)
     ,@body
     (red:exec)))
