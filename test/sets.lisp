;;; test/sets.lisp

(in-package #:cl-ohm)

(5am:def-suite sets)
(5am:in-suite sets)

(define-ohm-model title ()
  :attributes ((name :indexp t
                     :uniquep t)))

(define-ohm-model aristocrate ()
  :sets ((titles :element-type title)))

(5am:test adding-elements
  (flush-db)
  (let ((duke (create 'aristocrate))
        (title (create 'title :name "Duke of Playground")))
    (add (titles duke) title)
    (5am:is (= 1 (size (titles duke))))
    (add (titles duke) title)
    (5am:is (= 1 (size (titles duke))))))

(5am:test removing-elements
  (flush-db)
  (let ((duke (create 'aristocrate))
        (title (create 'title :name "Duke of Playground")))
    (add (titles duke) title)
    (5am:is (= 1 (size (titles duke))))
    (remove (titles duke) title)
    (5am:is (zerop (size (titles duke))))))

(5am:test replaceing-elements
  (flush-db)
  (let ((duke (create 'aristocrate))
        (title (create 'title :name "Duke of Playground")))
    (add (titles duke) title)
    (5am:is (= 1 (size (titles duke))))
    (replace (titles duke) (list (create 'title) (create 'title)))
    (5am:is (= 2 (size (titles duke))))))

(5am:test finding-id-in-set
  (flush-db)
  (let ((duke (create 'aristocrate))
        (title (create 'title :name "Duke of Playground")))
    (add (titles duke) title)
    (5am:is (find-id (titles duke) (ohm-id title)))))

(5am:test set-elements
  (flush-db)
  (let ((duke (create 'aristocrate))
        (title (create 'title :name "Duke of Playground")))
    (add (titles duke) title)
    (5am:is (string= "Duke of Playground"
                     (name (car (elements (titles duke))))))))

(5am:test set-union
  (flush-db)
  (create 'person :first-name "Steve" :country "USA")
  (create 'person :first-name "Dirk" :country "Germany")
  (5am:is (= 1 (size (filter 'person :first-name "Steve"))))
  (5am:is (= 1 (size (filter 'person :country "Germany"))))
  (5am:is (= 2
             (size (union (filter 'person :first-name "Steve")
                          (filter 'person :country "Germany"))))))

(5am:test set-combine
  (flush-db)
  (create 'person :first-name "Steve" :country "USA")
  (create 'person :first-name "Steve" :country "France")
  (5am:is (= 2 (size (filter 'person :first-name "Steve"))))
  (5am:is (= 1
             (size (combine (filter 'person :first-name "Steve")
                            (filter 'person :country "USA"))))))

(5am:test set-except
  (flush-db)
  (create 'person :first-name "Steve" :country "USA")
  (create 'person :first-name "Steve" :country "France")
  (5am:is (= 2 (size (filter 'person :first-name "Steve"))))
  (5am:is (= 1
             (size (except (filter 'person :first-name "Steve")
                           (filter 'person :country "USA"))))))
