;;; test/lists.lisp

(in-package #:cl-ohm)

(5am:def-suite lists)
(5am:in-suite lists)

(define-ohm-model tag ()
  :attributes ((name :indexp t)))

(define-ohm-model post ()
  :attributes ((title :indexp t))
  :lists ((tags :element-type tag)))

(5am:test adding-elements
  (flush-db)
  (let ((post (create 'post :title "foo")))
    (add (tags post) (create 'tag))
    (add (tags post) (create 'tag))
    (add (tags post) (create 'tag))

    (5am:is (= 3 (size (tags post))))))

(5am:test adding-elements-from-left
  (flush-db)
  (let ((post (create 'post :title "foo")))
    (add (tags post) (create 'tag :name "baz"))
    (add-left (tags post) (create 'tag :name "quux"))

    (5am:is (string= "quux"
                     (name (first (tags post)))))))

(5am:test remove
  (flush-db)
  (let ((post (create 'post :title "foo"))
        (tag (create 'tag :name "bar")))
    (add (tags post) tag)
    (5am:is (= 1 (size (tags post))))
    (remove (tags post) tag)
    (5am:is (zerop (size (tags post))))))

(5am:test error-on-unpersistable-object
  (flush-db)
  (let ((post (create 'post)))
    (5am:signals ohm-missing-id-error
      (add (tags post) (make-instance 'tag)))))

(5am:test pop
  (flush-db)
  (let ((post (create 'post)))
    (add (tags post) (create 'tag :name "foo"))
    (add (tags post) (create 'tag :name "bar"))
    (5am:is (string= "bar"
                     (name (pop (tags post)))))))

(5am:test pop-left
  (flush-db)
  (let ((post (create 'post)))
    (add (tags post) (create 'tag :name "foo"))
    (add (tags post) (create 'tag :name "bar"))
    (5am:is (string= "foo"
                     (name (pop-left (tags post)))))))

(5am:test index-first-last
  (flush-db)
  (let ((post (create 'post)))
    (add (tags post) (create 'tag :name "foo"))
    (add (tags post) (create 'tag :name "quux"))
    (add (tags post) (create 'tag :name "bar"))

    (5am:is (string= "foo"
                     (name (first (tags post)))))
    (5am:is (string= "quux"
                     (name (index (tags post) 1))))
    (5am:is (string= "bar"
                     (name (last (tags post)))))))

(5am:test list-ids
  (flush-db)
  (let ((post (create 'post)))
    (add (tags post) (create 'tag :name "foo"))
    (add (tags post) (create 'tag :name "quux"))
    (add (tags post) (create 'tag :name "bar"))

    (5am:is (equal (list "2" "3" "4")
                   (list-ids (tags post))))))

(5am:test list-member
  (flush-db)
  (let ((post (create 'post))
        (tag (create 'tag)))
    (add (tags post) tag)
    (5am:is (member (tags post) tag))))

(5am:test range
  (flush-db)
  (let ((post (create 'post)))
    (add (tags post) (create 'tag :name "foo"))
    (add (tags post) (create 'tag :name "quux"))
    (add (tags post) (create 'tag :name "bar"))

    (5am:is (= 2
               (length (range (tags post) 1 2))))))

(5am:test replace
  (flush-db)
  (let ((post (create 'post)))
    (add (tags post) (create 'tag :name "foo"))
    (5am:is (string= "foo" (name (first (tags post)))))
    (replace (tags post) (list (create 'tag :name "bar")))
    (5am:is (string= "bar" (name (first (tags post)))))))
