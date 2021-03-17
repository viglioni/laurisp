;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; global related functions
;;


;;
;; flycheck
;;

(global-flycheck-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;
;; company-mode
;;
(global-company-mode)

;;
;; shell
;;

(setq sh-indentation 2)
(exec-path-from-shell-initialize)

;; open dotfiles with sh-mode
(add-to-list 'auto-mode-alist '("/\\.[a-zA-Z0-09]*rc$" . sh-mode))
(add-to-list 'auto-mode-alist '("/\[a-zA-Z0-09]*rc$" . sh-mode))

;;
;; yasnippets 
;;

(add-to-list 'yas-snippet-dirs "~/laurisp/snippets")
(yas-global-mode 1)
