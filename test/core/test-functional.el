(load "./core/l-functional.el")

(describe
 "#compose-and-call - these suits also cover indirectly #compose and #curry"
 (it "should compose functions in the correct order"
     (expect (compose-and-call
              ((+ 1)
               (* 2)) 10))
     :to-be 21)

 (it "should compose seq functions correctly"
     (let ((test-list '(1 2 3 4 5 6 7 8 9 10))
           (sum (lambda (lst) (seq-reduce '+ lst 0)))
           (expected 55))
       (expect (compose-and-call
                ((funcall sum)
                 (seq-concatenate 'list '(1 2 3 4))
                 (seq-map (lambda (x) (+ 1 x)))
                 (seq-filter (lambda (el) (> el 5)))) test-list)
               :to-be expected)))

 (it "should properly compose functions with regex params"
     (let ((regexp "foo")
           (str-replace "bar"))
       (expect (compose-and-call
                ((replace-regexp-in-string regexp str-replace))
                "foo bar") :to-equal "bar bar"))))

(describe
 "#thread-last ->"
 (it "should pipe functions in the correct order"
     (expect (-> 10
              ((+ 1)
               (* 2)))
     :to-be 22)))

(describe
 "#compose"
 (it "should compose in the correct order"
     (expect
      (funcall  (compose (+ 1) (* 2)) 10)
      :to-equal 21)))

(describe
 "#curry"
 (it "should curry correctly when there are several arguments"
     (expect
      (funcall (curry + 1 2 3 4 5) 6 7 8 9 10)
      :to-be 55)))
