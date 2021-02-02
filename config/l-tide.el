;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; tide related functions
;;



;;
;; Keybinds inside tide-mode
;;

(eval-after-load 'typescript-mode
  (lambda ()
    ;; New prefixes for commands
    (spacemacs/declare-prefix-for-mode 'typescript-mode "me" "errors")
    (spacemacs/declare-prefix-for-mode 'typescript-mode "mf" "format")
    ;; format 
    (spacemacs/set-leader-keys-for-major-mode 'typescript-mode "ff" 'tide-fix)
    (spacemacs/set-leader-keys-for-major-mode 'typescript-mode "fo" 'tide-organize-imports)
    (spacemacs/set-leader-keys-for-major-mode 'typescript-mode "f=" 'tide-format)
    (spacemacs/set-leader-keys-for-major-mode 'typescript-mode "fr" 'tide-refactor)
    ;; rename
    (spacemacs/set-leader-keys-for-major-mode 'typescript-mode "rf" 'tide-rename-file)
    ;; errors
    (spacemacs/set-leader-keys-for-major-mode 'typescript-mode "ep" 'tide-error-at-point)
    (spacemacs/set-leader-keys-for-major-mode 'typescript-mode "en" 'flycheck-next-error)
    ))

(eval-after-load 'rjsx-mode
  (lambda ()
    ;; New prefixes for commands
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "me" "errors")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mf" "format")
    ;; format 
    (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode "ff" 'tide-fix)
    (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode "fo" 'tide-organize-imports)
    (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode "f=" 'tide-format)
    (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode "fr" 'tide-refactor)
    ;; rename
    (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode "rf" 'tide-rename-file)
    ;; errors
    (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode "ep" 'tide-error-at-point)
    (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode "en" 'flycheck-next-error)
    ))

(eval-after-load 'web-mode
  (lambda ()
    ;; New prefixes for commands
    (spacemacs/declare-prefix-for-mode 'web-mode "me" "errors")
    (spacemacs/declare-prefix-for-mode 'web-mode "mf" "format")
    ;; format 
    (spacemacs/set-leader-keys-for-major-mode 'web-mode "ff" 'tide-fix)
    (spacemacs/set-leader-keys-for-major-mode 'web-mode "fo" 'tide-organize-imports)
    (spacemacs/set-leader-keys-for-major-mode 'web-mode "f=" 'tide-format)
    (spacemacs/set-leader-keys-for-major-mode 'web-mode "fr" 'tide-refactor)
    ;; rename
    (spacemacs/set-leader-keys-for-major-mode 'web-mode "rf" 'tide-rename-file)
    ;; errors
    (spacemacs/set-leader-keys-for-major-mode 'web-mode "ep" 'tide-error-at-point)
    (spacemacs/set-leader-keys-for-major-mode 'web-mode "en" 'flycheck-next-error)
    ))

(add-hook
 'tide-mode-hook
 (lambda ()
   ;; Repl
   (local-set-key (kbd "C-x C-e") 'ts-send-last-sexp)
   (local-set-key (kbd "C-c C-b") 'ts-send-buffer)
   ))


;;
;; hooks
;;
(add-hook 'before-save-hook 'tide-format-before-save)


;;
;; rjsx extra configs
;;
(add-to-list 'auto-mode-alist '("\\.js.*$" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-hook 'rjsx-mode-hook 'tide-setup-hook)

;;
;; web-mode extra config
;;
(add-hook 'web-mode-hook 'tide-setup-hook
          (lambda () (pcase (file-name-extension buffer-file-name)
                  ("tsx" ('tide-setup-hook))
                  (_ (my-web-mode-hook)))))
(flycheck-add-mode 'typescript-tslint 'web-mode)
(add-hook 'web-mode-hook 'company-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook #'turn-on-smartparens-mode t)




