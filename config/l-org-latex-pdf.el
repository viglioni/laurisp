;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; org, latex or pdf related functions
;;


;;
;; ORG
;;

;; dont ask before running code
(setq org-confirm-babel-evaluate nil)

(eval-after-load "ox-latex"
  (lambda ()
    ;; add ic-tese-v3 class
    (add-to-list
     'org-latex-classes
     '("ic-tese-v3" "\\documentclass{ic-tese-v3}"
       ("\\chapter{%s}" . "\\chapter*{%s}")
       ("\\section{%s}" . "\\section*{%s}")
       ("\\subsection{%s}" . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
    ))

(eval-after-load "org"
  (lambda ()
    (progn
      ;; add languages to list
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((haskell . t)
         (clojure . t)
         (emacs-lisp . t)
         (python . t)
         (js . t)
         (C . t)
         (latex . t)
         (shell . t)
         (sql . t)))
      ;; disable fullpage preview
      ;(add-to-list 'org-latex-packages-alist '("" "fullpage" t))
      ;; highlight latex
      (setq org-highlight-latex-and-related '(latex script entities))
      ;; org-mode startup
      (setq org-startup-folded t)
      (setq org-startup-with-latex-preview t)
      ;; add latex commands inside major mode
      (spacemacs/declare-prefix-for-mode 'org-mode "\\" "LaTeX")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "\\t" 'org-toggle-latex-fragment
        "\\b" 'org-beamer-export-to-pdf
        "\\e" 'LauTeX-org-env
        "\\p" 'compile-org-to-pdf
        "\\r" 'LauTeX-insert-reference
        "\\c" 'LauTeX-insert-citation
        "\\s" 'preview-latex-on-section
        "\\b" 'preview-latex-on-buffer
        ;; bind hide entry
        "hh" 'org-hide-entry
        ;; insert src code
        "ic" 'insert-org-source))))


(add-hook
 'org-mode-hook
 (lambda ()
   ;; key-bindings
   (local-set-key (kbd "C-<up>") (define-org-cmd
                                   :heading 'org-move-subtree-up
                                   :table 'org-table-move-row-up))
   (local-set-key (kbd "C-<down>") (define-org-cmd
                                     :heading 'org-move-subtree-down
                                     :table 'org-table-move-row-down))
   (local-set-key (kbd "C-<left>") (define-org-cmd
                                     :heading 'org-promote-subtree
                                     :table 'org-table-move-column-left))
   (local-set-key (kbd "C-<right>") (define-org-cmd
                                      :heading 'org-demote-subtree
                                      :table 'org-table-move-column-right))
   ;; force line breaks
   (visual-line-mode)
   ;; define latex preview colours and scale
   (latex-define-preview-settings)
   ))



;;
;; PDF
;;

(add-hook 'pdf-view-mode-hook (lambda () (linum-mode 0)))



;;
;; LaTeX
;;

(add-hook 'latex-mode 'visual-line-mode)

(eval-after-load "tex-fold"
  (lambda ()
    (progn
      ;; fold buffer on opening
      (add-hook 'find-file-hook 'TeX-fold-buffer t)
      ;; fold automatically
      (setq TeX-fold-auto t)
      ;; fold these envs:
      (add-to-list 'TeX-fold-env-spec-list '("[definition]" ("definition")))
      (add-to-list 'TeX-fold-env-spec-list '("[lemma]" ("lemma")))
      (add-to-list 'TeX-fold-env-spec-list '("[theorem]" ("theorem")))
      (add-to-list 'TeX-fold-env-spec-list '("[example]" ("example")))
      )))


(spacemacs/declare-prefix-for-mode 'latex-mode "o" "org-edit-functions")
(spacemacs/set-leader-keys-for-major-mode 'latex-mode
  "op" 'LauTeX-preview-org-env
  "oe" 'LauTeX-org-env-exit
  "or" 'LauTeX-insert-reference)


