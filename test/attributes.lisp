;;; test/attributes.lisp

(in-package #:cl-ohm)

(5am:def-suite attributes)
(5am:in-suite attributes)

(define-ohm-model person ()
  :attributes ((first-name :indexp t)
               (last-name :indexp t)
               (country :indexp t)
               eye-color))

(5am:test setting-and-getting-attributes
  (flush-db)
  (let ((person (create 'person
                        :first-name "John"
                        :last-name "McCarthy"
                        :country "USA"
                        :eye-color "undefined")))
    (5am:is (string= (first-name person)
                     (first-name (filter-id 'person (ohm-id person)))))

    (5am:is (string= (first-name person)
                     (first-name (cl:first (elements (filter 'person :first-name "John"))))))

    (5am:is (string= (first-name person)
                     (first-name (cl:first (elements (filter 'person :last-name "McCarthy"))))))

    (5am:is (string= (first-name person)
                     (first-name (cl:first (elements (filter 'person :country "USA"))))))

    (5am:signals ohm-no-index-found-error
      (filter 'person :eye-color "green"))))

(5am:test chaning-attrinutes
  (flush-db)
  (let ((bill (create 'person :first-name "Bill")))
    (5am:is (string= "Bill" (first-name (filter-id 'person (ohm-id bill)))))
    (setf (first-name bill) "William")
    (save bill)
    (5am:is (string= "William" (first-name (filter-id 'person (ohm-id bill)))))))

(5am:test filtering
  (flush-db)
  (create 'person
          :first-name "Frank"
          :last-name "Miller"
          :country "USA")
  (create 'person
          :first-name "Frank"
          :last-name "Müller"
          :country "Germany")
  (create 'person
          :first-name "Mario"
          :last-name "Mario"
          :country "Italy")

  (5am:is (= 3
             (size (filter 'person))))
  (5am:is (= 2
             (size (filter 'person :first-name "Frank"))))
  (5am:is (= 1
             (size (filter 'person :first-name "Frank" :country "Germany")))))

(5am:test deleting
  (flush-db)
  (let ((bill (create 'person :first-name "Bill")))
    (5am:is (filter-id 'person (ohm-id bill)))
    (del bill)
    (5am:is (not (filter-id 'person (ohm-id bill))))))

(define-ohm-model user (person)
  :attributes ((email :uniquep t)))

(5am:test unique-values
  (flush-db)
  (create 'user :first-name "Dick" :email "e@example.org")
  (5am:is (string= "Dick"
                   (first-name (filter-with 'user :email "e@example.org"))))
  (5am:signals ohm-unique-constraint-violation
    (create 'user :email "e@example.org")))
