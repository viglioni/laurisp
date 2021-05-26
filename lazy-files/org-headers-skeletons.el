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
  (declare (indent defun))
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
  "#+begin_src emacs-lisp :var strr=\"\" :exports none"
  "(format \"%s\" (replace-regexp-in-string (rx (and (| \"*Main\" \"Prelude\") (? \"|\") (? white) (? \">\") (? white))) \"\" (format \"%s\" strr)))"
  "#+end_src")

(create-skeleton org-clojure-notebook-header
  "header for clojure notebooks"
  "#+Title:"
  "#+startup: fold"
  "#+name: org-clear-clojure-output"
  "#+begin_src emacs-lisp :var strr=\"\" :exports none"
  "(replace-regexp-in-string (rx (| (and \"(#'user\" (+ (not \")\")) \")\" (+ white)) (and line-start \"(\") (and \")\" line-end))) \"\" (format "%s" strr))"
  "#+end_src")


(create-skeleton org-beamer-presentations-header
  "header for beamer presentations"
  "#+title:"
  "#+date:"
  "#+author:"
  "#+email:"
  "#+language:"
  "#+select_tags: export"
  "#+exclude_tags: noexport"
  "#+startup: beamer"
  "#+LaTeX_CLASS: beamer"
  "#+LaTeX_CLASS_OPTIONS: [bigger]"
  "#+beamer_theme: metropolis"
  "#+options: tex:t toc:nil H:2")

(create-skeleton org-latex-article-header
  "header for articles"
  "#+title:"
  "#+author:"
  "#+date:"
  "#+language: en"
  "#+latex_compiler:"
  "#+OPTIONS: tex:t  toc:nil todo:nil"
  "#+STARTUP: nolatexpreview fold"
  "#+EXCLUDE_TAGS: noexport"
  "#+latex_class: article"
  "#+latex_class_options: [a4paper,12pt]")

(provide 'org-headers-skeletons)
