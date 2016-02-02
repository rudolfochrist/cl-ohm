;;;; cl-ohm.asd

(asdf:defsystem #:cl-ohm
  :description "Describe cl-ohm here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-redis
               #:closer-mop
               #:alexandria)
  :serial t
  :components ((:file "package")
               (:file "connection")
               (:file "conditions")
               (:file "mop")
               (:file "ohm-counter")
               (:file "ohm-object")
               (:file "cl-ohm")))
