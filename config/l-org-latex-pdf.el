;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; org, latex or pdf related configs
;;


;;
;; ORG
;;

(with-eval-after-load "ob-core"
  ;; dont ask before running code
  (setq org-confirm-babel-evaluate nil)

  ;; set nodepath to use libs in org-babel
  (setenv "NODE_PATH"
          (concat (getenv "HOME") "/org-babel-config/js/node_modules" ":"
                  (getenv "NODE_PATH"))))

(with-eval-after-load "ox-latex"
  ;; add ic-tese-v3 class
  (add-to-list
   'org-latex-classes
   '("ic-tese-v3" "\\documentclass{ic-tese-v3}"
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(with-eval-after-load "org"
  (load-lib 'laurg)
  (load-lib 'lautex)
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

  ;; highlight latex
  (setq org-highlight-latex-and-related '(latex script entities))

  ;; org-mode startup
  (setq org-startup-folded t)
  (setq org-startup-with-latex-preview nil)

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
    "ic" 'insert-org-source
    ;; insert custom headings
    "ia" 'org-insert-custom-headers))


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
   ;; center buffer
   (olivetti-mode)
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

(with-eval-after-load "tex-fold"
  ;; fold buffer on opening
  (add-hook 'find-file-hook 'TeX-fold-buffer t)

  ;; fold automatically
  (setq TeX-fold-auto t)

  ;; fold these envs:
  (add-to-list 'TeX-fold-env-spec-list '("[definition]" ("definition")))
  (add-to-list 'TeX-fold-env-spec-list '("[lemma]" ("lemma")))
  (add-to-list 'TeX-fold-env-spec-list '("[theorem]" ("theorem")))
  (add-to-list 'TeX-fold-env-spec-list '("[example]" ("example"))))


(spacemacs/declare-prefix-for-mode 'latex-mode "o" "org-edit-functions")
(spacemacs/set-leader-keys-for-major-mode 'latex-mode
  "op" 'LauTeX-preview-org-env
  "oe" 'LauTeX-org-env-exit
  "or" 'LauTeX-insert-reference)


