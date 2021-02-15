;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; functional related functions
;;

(require 'seq)
(require 'cl)

;;
;; Function
;;

(defmacro curry-expr (expr)
  "Curries an expression
   e.g. (curry-expr '(+ 1 2 3)) -> (curry + 1 2 3)"
  `(eval (seq-concatenate 'list '(curry) ,expr)))

(defmacro compose (&rest fn-list)
  "Compose functions (and curries them) from right to left.
   ((y -> z) ... (m -> o) ((a ... n) -> m) ) -> ((a ... n)->z)
   e.g.:
   (compose (+ 1) (* 2)) -> (lambda (arg1 ... argN) (+ 1 (* 2 arg1 ... argN)))"
  `(let ((curried-fn (quote ,(seq-map (lambda (fn) (curry-expr fn)) fn-list))))
     (reduce
      (lambda (f g)
        (lexical-let ((f f) (g g))
          (lambda (&rest args) (funcall f (apply g args)))))
      curried-fn
      :initial-value (curry identity))))


;;;###autoload
(defmacro curry (fn &rest initial-args)
  "Returns the curried function.
   ((* -> x) arg1, ..., argN) -> ((argN+1, ..., argM) -> x)
   e.g.:
   (curry + 1 2 3) -> (lambda (argN ... argM) (+ 1 2 3 argN ... argM))"
  `(lambda (&rest args)
     (apply (quote ,fn) (seq-concatenate 'list (list ,@initial-args) args))))

;;;###autoload
(defmacro -> (arg fn-list)
  "Pipe an argument into composed functions from left to right.
   a -> ((a -> b) (b -> c) ... (n -> m)) -> m
   e.g.:
   (-> (+ 1) (* 2)) -> (lambda (arg1 ... argN) (* 2 (+ 1 arg1 ... argN)))"
  `(funcall (compose ,@(reverse  fn-list)) ,arg))

;;;###autoload
(defmacro compose-and-call (fn-list &rest args)
  "Since compose returns a function, this helper receives a list of
   functions and args and apply them to composed funcs
   (a ... n) -> ((y -> z) ... (m -> o) ((a ... n) -> m) ) -> z
    e.g.:
    (compose-and-call ((+ 1) (* 2)) 2 3) -> 13"
  `(funcall (compose ,@fn-list) ,@args))

(defun identity (arg)
  "Identity function.
   a -> a"
  arg)

;;
;; Logic 
;;

;;;###autoload
(defun bool (x)
  "returns t if element is truthy, nil if its falsey
   a -> bool"
  (not (not x)))

;;;###autoload
(defun n-and (&rest args)
  "Not and.
   * -> bool"
  (not (all args)))

;;;###autoload
(defun n-or (&rest args)
  "Not or.
   * -> bool"
  (not (any args)))

;;
;; List 
;;

;;;###autoload
(defun all (lst)
  "Returns t if all elements in list are truthy
   [a] → Boolean"
  (bool (seq-reduce
         (lambda (acc val) (and acc val))
         lst t)))

;;;###autoload
(defun any (lst)
  "Returns t if at least one element in list is truthy
   [a] → Boolean"
  (bool (seq-reduce
         (lambda (acc val) (or acc val))
         lst nil)))

;;;###autoload
(defun contains? (list element)
  "Returns t/nil if element is in list
   ([a] a) -> bool"
  (bool (member element list)))

;;;###autoload
(defun head (list)
  "Returns the first element of a list
   [a] -> a | nil"
  (car list))

;;;###autoload
(defun not-contains? (list element)
  "Returns t/nil if element is not in list
   ([a] a) -> bool"
  (if list (not (contains? list element))))

;;;###autoload
(defun tail (list)
  "Returns the list but its first element
   [a] -> [a] | nil"
  (cdr list))

;;;###autoload
(defun unzip (zipped-list)
  "unzip n lists
   [[a]] -> [[a]]
   e.g.:
   (unzip '((1 2 ... n) (1 2 ... n))) ->
   '((1 1 ... 1) (2 2 .... 2) ... (n n ... n)"
  (if (and zipped-list (all zipped-list))
      (let ((heads (mapcar* 'head zipped-list))
            (tails (mapcar* 'tail zipped-list)))
        (append (list heads) (unzip tails)))))

;;;###autoload
(defun zip (&rest lists)
  "zips n lists
   [[a]] -> [[a]]
   e.g.:
   (zip '(1 1) '(2 2) '(3 3)) -> '((1 2 3) (1 2 3))"
  (apply (curry  mapcar* 'list) lists))

;;
;; Number
;;

;;;###autoload
(defun inc (n)
  "Returns the increment of n
   Number -> Number"
(+ 1 n))

;;
;; Type
;;

;;;###autoload
(defun all-nil? (&rest args)
  "return if all args are nil
   (* ... *) -> boolean
   e.g (all-nil? nil nil) -> t"
  (apply 'n-or args))

(defun any-nil? (&rest args)
  "return if any args are nil
   (* ... *) -> boolean
   e.g (any-nil? nil t nil) -> t"
  (apply 'n-and args))


;;
;; alist
;;
;TODO: test it
;;;###autoload
(defun alist-sort-by-car (alist)
  (sort alist (lambda (a b) (string< (car a) (car b)))))



(provide 'l-functional)

