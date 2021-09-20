;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;


;;
;; Lazy binds from personal libs
;;

;; laurg
(bind-lazy-function 'insert-org-source 'laurg-insert-org-source 'laurg)
(bind-lazy-function 'insert-custom-headers 'laurg-insert-custom-headers 'laurg)
;; lautex
(bind-lazy-function 'preview-latex-on-buffer 'LauTeX-preview-latex-on-buffer 'lautex)
(bind-lazy-function 'preview-latex-on-section 'LaTeX-preview-latex-on-section 'lautex)
(bind-lazy-function 'compile-org-to-pdf 'LauTeX-compile-org-to-pdf 'lautex)
(bind-lazy-function 'org-env 'LauTeX-org-env 'lautex)
(bind-lazy-function 'org-env-exit 'LauTeX-org-env-exit 'lautex)
(bind-lazy-function 'preview-org-env 'LauTeX-preview-org-env 'lautex)
(bind-lazy-function 'insert-reference 'LauTeX-insert-reference 'lautex)
(bind-lazy-function 'insert-citation 'LauTeX-insert-citation 'lautex)

;;
;; ORG
;;
;;
;; Define org command by cursor position
;;

;;;###autoload
(defmacro define-org-cmd (&rest plist)
  "Receives a plist (:situation 'command)  as args to define which
   command should be called on each situation. 
   Obs.: the command will ONLY be called on the specific situation.
   *~*~*
   For now the supported keys are
   :heading -> runs when cursor is over a heading
   :table -> runs when cursor is over a table
   *~*~*
   example: (define-org-cmd :heading 'my-fn :table 'my-fn2)"
  (throw-if (oddp (length plist)) "arg list must have an even number of args")
  `(lambda ()
     (interactive)
     (cond
      ((org-at-heading-p) (funcall ,(plist-get plist :heading)))
      ((org-at-table-p)   (funcall ,(plist-get plist :table))))))


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


(setq haskell-process-type 'stack-ghci)

(with-eval-after-load "org"
  ;; load extra configs to org mode
  (org-babel-lob-ingest (join-path
                         laurisp-config-dir
                         "org-mode-extra-configs.org"))

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
    "\\e" 'org-env
    "\\p" 'compile-org-to-pdf
    "\\r" 'insert-reference
    "\\c" 'insert-citation
    "\\s" 'preview-latex-on-section
    "\\b" 'preview-latex-on-buffer
    ;; bind hide entry
    "hh" 'org-hide-entry
    ;; insert src code
    "ic" 'insert-org-source
    ;; insert custom headings
    "ia" 'insert-custom-headers))

;;;###autoload
(defun latex-define-preview-settings (&optional img-scale)
  "Define latex format options using the theme"
  (interactive)
  (let* ((foreground-color (face-attribute 'default :foreground))
         (background-color (face-attribute 'default :background))
         (text-scale (float (if (boundp 'text-scale-mode-amount) text-scale-mode-amount 0)))
         (minimum-scale (or img-scale 13))
         (scale (/  (float (+ minimum-scale text-scale)) 10)))
    (plist-put org-format-latex-options :scale scale)
    (plist-put org-format-latex-options :foreground foreground-color)
    (plist-put org-format-latex-options :background background-color)))

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
   (setq olivetti-minimum-body-width 100)
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
  "op" 'preview-org-env
  "oe" 'org-env-exit
  "or" 'insert-reference)


