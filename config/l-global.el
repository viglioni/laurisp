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

(setq lsp-idle-delay 0.5)
(setq gc-cons-threshold 200000000)
(setq lsp-auto-guess-root t)

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

(load-lib 'env-private)
(setq sh-indentation 2)

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




;;
;; language scripts
;;

(bind-lazy-function 'npm-choose-and-run 'npm-scripts:choose-and-run 'npm-scripts)
(bind-lazy-function 'hide-script-buffer 'lang-scripts:hide-buffer 'lang-scripts)
(bind-lazy-function 'open-script-buffer 'lang-scripts:open-active-buffer 'lang-scripts)
(bind-lazy-function 'go-to-script-buffer 'lang-scripts:go-to-buffer 'lang-scripts)
(bind-lazy-function 'import-default-lib 'npm-scripts:import-default-lib 'npm-scripts)
(bind-lazy-function 'npm-install-lib 'npm-scripts:install 'npm-scripts)
(bind-lazy-function 'npm-install-dev-lib 'npm-scripts:install-dev 'npm-scripts)
(bind-lazy-function 'run-make-cmd 'make-scripts:run-command 'make-scripts)

(which-key-add-key-based-replacements "<f17> <f17>" "language scripts")
;; Common commands
(which-key-add-key-based-replacements "<f17> <f17> <f17>" "common commands")
(global-set-key (kbd "<f17> <f17> <f17> h") 'hide-script-buffer)
(global-set-key (kbd "<f17> <f17> <f17> o") 'open-script-buffer)
(global-set-key (kbd "<f17> <f17> <f17> g") 'go-to-script-buffer)
;; NPM scripts
(which-key-add-key-based-replacements "<f17> <f17> n" "npm scripts")
(global-set-key (kbd "<f17> <f17> n r") 'npm-choose-and-run)
(global-set-key (kbd "<f17> <f17> n i") 'import-default-lib)
(global-set-key (kbd "<f17> <f17> n l") 'npm-install-lib)
(global-set-key (kbd "<f17> <f17> n d") 'npm-install-dev-lib)
;; MAKE scripts
(which-key-add-key-based-replacements "<f17> <f17> m" "makefile scripts")
(global-set-key (kbd "<f17> <f17> m r") 'run-make-cmd)
 
