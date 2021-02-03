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
  (cl-reduce (lambda (f g)
               (lexical-let ((f f) (g g))
                 (lambda (&rest arguments)
                   (funcall f (apply g arguments)))))
             more-functions
             :initial-value function))

;;;###autoload
(defmacro curry (fn &rest initial-args)
  `(lambda (&rest args)
     (apply (quote ,fn) (seq-concatenate 'list (list ,@initial-args) args))))

;;;###autoload
(defmacro compose (&rest fn-list)
  `(let ((curried-fn (quote ,(seq-map (lambda (x) (seq-concatenate 'list '(curry) x)) fn-list))))
      (eval (seq-concatenate 'list '(_compose) curried-fn))))

;;;###autoload
(defmacro -> (arg fn-list)
  `(funcall (compose ,@(reverse  fn-list)) ,arg))

;;;###autoload
(defmacro compose-and-call (fn-list arg)
  `(funcall (compose ,@fn-list) ,arg))

(defun identity (arg) arg)

;;
;; Logic 
;;

;;;###autoload
(defun bool (x)
  "returns nil/t"
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
         lst t)))


;;;###autoload
(defun contains (list element)
  "returns t/nil if element is in list"
  (bool (member element list)))

;;;###autoload
(defun not-contains (list element)
  "returns t/nil if element is not in list"
  (not (contains list element)))

;;;###autoload
(defun head (list)
  ""
  (car list))

;;;###autoload
(defun tail (list)
  ""
  (cdr list))

;;;###autoload
(defun zip (&rest lists)
  "zips n lists"
  (apply (curry  mapcar* 'list) lists ))

;;;###autoload
(defun unzip (zipped-list &optional acc)
  "unzip n lists
   ((1 2 ... n) (1 2 ... n)) =>
   ((1 1 ... 1) (2 2 .... 2) ... (n n ... n)"
  (if (all zipped-list)
      (let ((heads (mapcar* 'head zipped-list))
            (tails (mapcar* 'tail zipped-list)))
        (append (list heads) (unzip tails)))))

