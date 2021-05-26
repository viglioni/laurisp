;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; global related functions
;;


;;
;; LSP
;;

;; (advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))

;; (advice-remove #'lsp #'(lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))
;; (advice--p (advice--symbol-function 'lsp))


;;
;; flycheck
;;
(with-eval-after-load "flycheck"
  (global-flycheck-mode))
;;(add-hook 'after-init-hook #'global-flycheck-mode)

;;
;; company-mode
;;
(with-eval-after-load "company"
  (global-company-mode))

;;
;; shell
;;

(setq sh-indentation 2)
(with-eval-after-load "exec-path-from-shell-initialize"
  (exec-path-from-shell-initialize))

;; open dotfiles with sh-mode
(add-to-list 'auto-mode-alist '("/\\.[a-zA-Z0-09]*rc$" . sh-mode))
(add-to-list 'auto-mode-alist '("/\[a-zA-Z0-09]*rc$" . sh-mode))

;;
;; yasnippets 
;;
(with-eval-after-load "yasnippet"
  (add-to-list 'yas-snippet-dirs "~/laurisp/snippets")
  (yas-global-mode 1))


;;
;; helm set jeys
;;

(global-set-key (kbd "<f19> <f19>") 'helm-M-x) 

;;
;; code-block set keys
;;

(with-eval-after-load "hideshow"
  (which-key-add-key-based-replacements "C-c b" "code-blocks")
  (global-set-key (kbd "C-c b t") 'hs-toggle-hiding)
  (global-set-key (kbd "C-c b l") 'hs-hide-level)
  (global-set-key (kbd "C-c b H") 'hs-hide-all)
  (global-set-key (kbd "C-c b S") 'hs-show-all)
  (global-set-key (kbd "C-c b s") 'hs-show-block)
  (global-set-key (kbd "C-c b h") 'hs-hide-block))

;;
;; window set keys
;;

;; walk through windows
(global-set-key (kbd "C-x <up>") 'evil-window-up)
(global-set-key (kbd "C-x <down>") 'evil-window-down)
(global-set-key (kbd "C-x <left>") 'evil-window-left)
(global-set-key (kbd "C-x <right>") 'evil-window-right)

;; resize windows
(global-set-key (kbd "C-c C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-c C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-c C-<up>") 'enlarge-window)
(global-set-key (kbd "C-c C-<down>") 'shrink-window)
