;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;


;;
;; tide related functions
;;



;; make tide import with relative paths
(eval-after-load "tide"
  (lambda ()
    (plist-put tide-user-preferences :importModuleSpecifierPreference "relative" )))

;;
;; Keybinds inside tide-mode
;;

(eval-after-load 'typescript-mode
  (lambda ()
    ;; New prefixes for commands
    (spacemacs/declare-prefix-for-mode 'typescript-mode "me" "errors")
    (spacemacs/declare-prefix-for-mode 'typescript-mode "mf" "format")
    (spacemacs/set-leader-keys-for-major-mode 'typescript-mode
      ;; format 
      "ff" 'tide-fix
      "fo" 'tide-organize-imports
      "fi" 'ls-import-js-libs
      "f=" 'tide-format
      "fr" 'tide-refactor
      ;; rename
      "rf" 'tide-rename-file
      ;; errors
      "ep" 'ls-explain-error-at-point
      "en" 'flycheck-next-error)))

(eval-after-load 'rjsx-mode
  (lambda ()
    ;; New prefixes for commands
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "me" "errors")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mf" "format")
    (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode
      ;; format 
      "ff" 'tide-fix
      "fo" 'tide-organize-imports
      "fi" 'ls-import-js-libs
      "f=" 'tide-format
      "fr" 'tide-refactor
      ;; rename
      "rf" 'tide-rename-file
      ;; errors
      "ep" 'ls-explain-error-at-point
      "en" 'flycheck-next-error)))

(eval-after-load 'web-mode
  (lambda ()
    ;; New prefixes for commands
    (spacemacs/declare-prefix-for-mode 'web-mode "me" "errors")
    (spacemacs/declare-prefix-for-mode 'web-mode "mf" "format")
    (spacemacs/declare-prefix-for-mode 'web-mode "mi" "HTML tags")
    (spacemacs/declare-prefix-for-mode 'web-mode "mb" "Fold/Unfold tags")

    (spacemacs/set-leader-keys-for-major-mode 'web-mode
      ;; web mode feats
      "ii" 'web-mode-element-insert-at-point
      "iv" 'web-mode-element-vanish
      "ik" 'web-mode-element-kill
      "is" 'web-mode-element-select
      "iw" 'web-mode-element-wrap
      "ir" 'web-mode-element-rename
      "ic" 'web-mode-element-clone
      "i/" 'web-mode-element-close
      "n" 'web-mode-element-next
      "p" 'web-mode-element-previous
      "bf" 'web-mode-fold-or-unfold
      "bc" 'web-mode-element-children-fold-or-unfold
      ;; format 
      "ff" 'tide-fix
      "fo" 'tide-organize-imports
      "fi" 'ls-import-js-libs
      "f=" 'tide-format
      "fr" 'tide-refactor
      ;; rename
      "rf" 'tide-rename-file
      ;; errors
      "ep" 'ls-explain-error-at-point
      "en" 'flycheck-next-error
      ;; autocomplete
      "c" 'company-tide)))

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
;; Use typescript-mode in TSX files
;;

;; (add-to-list 'auto-mode-alist '("\\.tsx$" . typescript-mode))

;;
;; web-mode extra config
;;
(add-to-list 'auto-mode-alist '("\\.tsx$" . web-mode))
(add-hook 'web-mode-hook 'tide-setup-hook
          (lambda () (pcase (file-name-extension buffer-file-name)
                       ("tsx" ('tide-setup-hook))
                       (_ (my-web-mode-hook)))))
(flycheck-add-mode 'typescript-tslint 'web-mode)
(add-hook 'web-mode-hook 'company-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook #'turn-on-smartparens-mode t)
