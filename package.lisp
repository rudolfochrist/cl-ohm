;;;; package.lisp

(defpackage #:cl-ohm
  (:nicknames #:ohm)
  (:use #:cl)
  (:shadow #:remove #:pop #:first #:last #:member
           #:replace #:union #:sort)
  (:import-from #:redis
                #:with-pipelining)
  (:import-from #:alexandria
                #:define-constant
                #:make-keyword
                #:once-only))
