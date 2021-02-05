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

;;;###autoload
(defsubst _compose (function &rest more-functions)
  "helper function to #compose"
  (cl-reduce (lambda (f g)
               (lexical-let ((f f) (g g))
                 (lambda (&rest arguments)
                   (funcall f (apply g arguments)))))
             more-functions
             :initial-value function))

;;;###autoload
(defmacro curry (fn &rest initial-args)
  "Returns the curried function.
   ((* -> x) arg1, ..., argN) -> ((argN+1, ..., argM) -> a)
   e.g.:
   (curry + 1 2 3) -> (lambda (argN ... argM) (+ 1 2 3 argN ... argM))"
  `(lambda (&rest args)
     (apply (quote ,fn) (seq-concatenate 'list (list ,@initial-args) args))))

;;;###autoload
(defmacro compose (&rest fn-list)
  "Compose functions (and curries them) from right to left.
   ((y -> z) ... (m -> o) ((a ... n) -> m) ) -> ((a ... n)->z)
   e.g.:
   (compose (+ 1) (* 2)) -> (lambda (arg1 ... argN) (+ 1 (* 2 arg1 ... argN)))"
  `(let ((curried-fn (quote ,(seq-map (lambda (x) (seq-concatenate 'list '(curry) x)) fn-list))))
      (eval (seq-concatenate 'list '(_compose) curried-fn))))

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
(defun contains (list element)
  "Returns t/nil if element is in list
   ([a] a) -> bool"
  (bool (member element list)))

;;;###autoload
(defun head (list)
  "Returns the first element of a list
   [a] -> a | nil"
  (car list))

;;;###autoload
(defun not-contains (list element)
  "Returns t/nil if element is not in list
   ([a] a) -> bool"
  (if list (not (contains list element))))

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

