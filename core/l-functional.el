;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; functional related functions
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

