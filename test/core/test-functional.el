(load "./test/l-test-helpers.el")
(load-test-file "l-functional")

;;
;; Functions
;;

(test-suit "#compose-and-call - these suits also cover indirectly #compose and #curry"
  (it-should "compose functions in the correct order"
    (expect (compose-and-call
             ((+ 1)
              (* 2)) 10))
    :to-be 21)

  (it-should "compose seq functions correctly"
    (let ((test-list '(1 2 3 4 5 6 7 8 9 10))
          (sum (lambda (lst) (seq-reduce '+ lst 0)))
          (expected 55))
      (expect (compose-and-call
               ((funcall sum)
                (seq-concatenate 'list '(1 2 3 4))
                (seq-map (lambda (x) (+ 1 x)))
                (seq-filter (lambda (el) (> el 5)))) test-list)
              :to-be expected)))

  (it-should "properly compose functions with regex params"
    (let ((regexp "foo")
          (str-replace "bar"))
      (expect (compose-and-call
               ((replace-regexp-in-string regexp str-replace))
               "foo bar") :to-equal "bar bar"))))

(test-suit "#thread-last ->"
  (it-should "pipe functions in the correct order"
    (expect (-> 10
               ((+ 1)
                (* 2)))
            :to-be 22)))

(test-suit "#compose"
  (it-should "compose in the correct order"
    (expect
     (funcall  (compose (+ 1) (* 2)) 10)
     :to-equal 21)))

(test-suit "#curry"
  (it-should "curry correctly when there are several arguments"
    (expect
     (funcall (curry + 1 2 3 4 5) 6 7 8 9 10)
     :to-be 55)))

;;
;; List
;;

(test-suit "#all"
  (it-should "return nil if the list has at least one nil entry"
    (expect (all '(1 2 3 nil 4)) :to-be nil)
    (expect (all '(nil nil nil)) :to-be nil)
    (expect (all '("asd" nil 1 nil)) :to-be nil))
  (it-should "return t if all entries are truthy"
    (expect (all '(1 "ase" '(nil) '())) :to-be t)))

(test-suit "#any"
  (it-should "return t if the list has at least one truthy entry"
    (expect (any '(1 2 3 nil 4)) :to-be t)
    (expect (any '(nil nil nil 1)) :to-be t)
    (expect (any '("asd" nil 1 nil)) :to-be t))
  (it-should "return nil if all entries are nil"
    (expect (any '(nil nil nil)) :to-be nil)))

(test-suit "#contains"
  (it-should "return t if element is in the list"
    (expect (contains '(1 2 3) 1) :to-be t))
  (it-should "return nil if element is not in the list"
    (expect (contains '(1 2 3) 4) :to-be nil)))

(test-suit "#head"
  (it-should "return the first element when the list is not empty"
    (expect (head '(1 2 3)) :to-be 1)
    (expect (head '(1)) :to-be 1))
  (it-should "return nil if the list is empty"
    (expect (head '()) :to-be nil)
    (expect (head nil) :to-be nil)))

(test-suit "#not-contains"
  (it-should "return nil if element is in the list"
    (expect (not-contains '(1 2 3) 1) :to-be nil))
  (it-should "return t if element is not in the list"
    (expect (not-contains '(1 2 3) 4) :to-be t)))

(test-suit "#tail"
  (it-should "return nil if there is one elment"
    (expect (tail '(1)) :to-be nil))
  (it-should "return nil if list is empty"
    (expect (tail '()) :to-be nil))
  (it-should "return nil if list is nil"
    (expect (tail nil) :to-be nil))
  (it-should "return the (n-1)th elements of a list"
    (expect (tail '(1 2 3 4)) :to-equal '(2 3 4))))

(test-suit "#unzip"
  (context "list or sublists are empty"
    (it-should "return nil"
      (expect (unzip nil) :to-be nil)
      (expect (unzip '(nil nil nil)) :to-be nil)))
  (context "lists have different size"
    (it-should "unzip to the size of the smaller"
      (expect (unzip '((1 2) nil)) :to-be nil)
      (expect (unzip '((1 2) (1))) :to-equal '((1 1)))))
  (context "lists have the same size"
    (it-should "unzip properly"
      (expect (unzip '((1 2 3) (1 2 3))) :to-equal '((1 1) (2 2) (3 3))))))

(test-suit "#zip"
  (context "all lists are empty"
    (it-should "return one empty list"
      (expect (zip nil nil nil) :to-be nil)
      (expect (zip '() '() '()) :to-be nil)))
  (context "the lists have different sizes"
    (it-should "zip the size of the smaller list"
      (expect (zip '(1 2) '(1) nil) :to-be nil)
      (expect (zip '(1 2 3) '(1 2 3 4) '(1)) :to-equal '((1 1 1)))))
  (context "lists have the same length"
    (it-should "properly zip them"
      (expect (zip '(1 1 1 1) '(2 2 2 2) '(3 3 3 3) '(4 4 4 4))
              :to-equal '((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4)))))
  (context "lists have different types"
    (it-should "properly zip them"
      (expect (zip '(1 2) '("fst" "snd") '(("list one") ("list two")))
              :to-equal '((1 "fst" ("list one")) (2 "snd" ("list two")))))))

;;
;; Number
;;

(test-suit "#inc"
  (context "number is integer"
    (it-should "increment number"
      (expect (inc 1) :to-be 2)
      (expect (inc 0) :to-be 1)
      (expect (inc -1) :to-be 0)))
  (context "number is not intenger"
    (it-should "increment number"
      (expect (inc 1.1) :to-equal 2.1))))


