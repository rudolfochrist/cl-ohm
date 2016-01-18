;;;; cl-ohm.asd

(asdf:defsystem #:cl-ohm
  :description "Describe cl-ohm here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-redis
               #:closer-mop)
  :serial t
  :components ((:file "package")
               (:file "cl-ohm")))
