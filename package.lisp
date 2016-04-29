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
                #:once-only)
  ;; high-level
  (:export
   #:ohm-id
   #:class-key
   #:object-key
   #:create
   #:save
   #:del
   #:filter
   #:filter-id
   #:filter-with
   #:define-ohm-model)
  ;; conditions
  (:export
   #:ohm-missing-id-error
   #:ohm-no-index-found-error
   #:ohm-unique-constraint-violation)
  ;; connection
  (:export
   #:setup-redis-connection
   #:with-connection
   #:with-transaction
   #:flush-db)
  ;; counter
  (:export
   #:counter
   #:incr
   #:decr)
  ;; sets & lists
  (:export
   #:add
   #:remove
   #:replace
   #:size
   #:member
   #:elements
   #:add-left
   #:pop
   #:pop-left
   #:index
   #:first
   #:last
   #:list-ids
   #:range
   #:find-id
   #:set-ids
   #:union
   #:combine
   #:except
   #:sort
   #:sort-by))
