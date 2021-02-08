;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;


;;
;; paredit related functions
;;


(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode) 
(add-hook 'ielm-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)


(eval-after-load "smartparens"
  (lambda ()
    ;; other modules
    (sp-use-paredit-bindings)))


;;
;; emacs lisp
;;

(add-hook 'emacs-lisp-mode-hook 'erefactor-lazy-highlight-turn-on)
(add-hook 'lisp-interaction-mode-hook 'erefactor-lazy-highlight-turn-on)
(spacemacs/declare-prefix-for-mode 'emacs-lisp-mode "mf" "format")

(eval-after-load "erefactor"
  (lambda ()
    (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode "fL" 'erefactor-lint-by-emacsen)
    (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode "fR" 'erefactor-rename-symbol-in-package)
    (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode "fA" 'erefactor-add-current-defun)
    (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode "fc" 'erefactor-change-prefix-in-buffer)
    (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode "fd" 'erefactor-dehighlight-all-symbol)
    (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode "fh" 'erefactor-highlight-current-symbol)
    (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode "fl" 'erefactor-lint)
    (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode "fr" 'erefactor-rename-symbol-in-buffer)
    (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode "fx" 'erefactor-eval-current-defun)
    (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode "f?" 'erefactor-flymake-display-errors)))



