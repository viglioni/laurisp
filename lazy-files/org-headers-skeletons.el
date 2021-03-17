;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; Headers for org-mode
;;

;;;###autoload
(defmacro create-skeleton (skeleton-name doc &rest body)
  `(let ((formated-lines (quote ,(seq-map (lambda (line) (concat line "\n")) body))))
     (eval (seq-concatenate 'list
                            '(define-skeleton ,skeleton-name ,doc "")
                            formated-lines))))


;;;###autoload
(create-skeleton org-haskell-notebook-header
  "header for haskell notebooks"
  "#+Title:"
  "#+startup: fold"
  "#+name: org-clear-haskell-output"
  "#+begin_src emacs-lisp :var strr=\"\""
  "(format \"%s\" (replace-regexp-in-string \"\\*Main|? ?>? ?\" \"\" (format \"%s\" strr)))"
  "#+end_src")


(provide 'org-headers-skeletons)
