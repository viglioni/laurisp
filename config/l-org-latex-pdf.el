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
      ;; disable line count in presentation mode :)
      (add-hook 'epresent-mode-hook (lambda () (linum-mode 0)))
      ;; highlight latex
      (setq org-highlight-latex-and-related '(latex script entities))
      ;; set custom preview options for latex
      (plist-put org-format-latex-options :scale 1.3)
      (plist-put org-format-latex-options :foreground "White")
      ;; org-mode startup
      (setq org-startup-folded t)
      (setq org-startup-with-latex-preview t)
      ;; add latex commands inside major mode 
      (spacemacs/declare-prefix-for-mode 'org-mode "\\" "LaTeX")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode "\\t" 'org-toggle-latex-fragment)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode "\\b" 'org-beamer-export-to-pdf)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode "\\e" 'LauTeX-org-env)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode "\\p" 'org-latex-export-to-pdf)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode "\\r" 'LauTeX-insert-reference)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode "\\c" 'LauTeX-insert-citation)
      ;; bind hide entry
      (spacemacs/set-leader-keys-for-major-mode 'org-mode "hh" 'org-hide-entry)
      )))





(add-hook
 'org-mode-hook
 (lambda ()
   ;; key-bindings
   (local-set-key (kbd "C-<up>") 'org-move-subtree-up)
   (local-set-key (kbd "C-<down>") 'org-move-subtree-down)
   (local-set-key (kbd "C-<left>") 'org-promote-subtree)
   (local-set-key (kbd "C-<right>") 'org-demote-subtree)
   ;; force line breaks
   (spacemacs/toggle-visual-line-navigation-on)
   (setq-local word-wrap nil)
   ))



;;
;; PDF
;;

(add-hook 'pdf-view-mode-hook (lambda () (linum-mode 0)))



;;
;; LaTeX
;;

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
(spacemacs/set-leader-keys-for-major-mode 'latex-mode "op" 'LauTeX-preview-org-env)
(spacemacs/set-leader-keys-for-major-mode 'latex-mode "oe" 'LauTeX-org-env-exit)
(spacemacs/set-leader-keys-for-major-mode 'latex-mode "or" 'LauTeX-insert-reference)

