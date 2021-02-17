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
    ;; format 
    (spacemacs/set-leader-keys-for-major-mode 'typescript-mode "ff" 'tide-fix)
    (spacemacs/set-leader-keys-for-major-mode 'typescript-mode "fo" 'tide-organize-imports)
    (spacemacs/set-leader-keys-for-major-mode 'typescript-mode "fi" 'ls-import-js-libs)
    (spacemacs/set-leader-keys-for-major-mode 'typescript-mode "f=" 'tide-format)
    (spacemacs/set-leader-keys-for-major-mode 'typescript-mode "fr" 'tide-refactor)
    ;; rename
    (spacemacs/set-leader-keys-for-major-mode 'typescript-mode "rf" 'tide-rename-file)
    ;; errors
    (spacemacs/set-leader-keys-for-major-mode 'typescript-mode "ep" 'ls-explain-error-at-point)
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
    (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode "fi" 'ls-import-js-libs)
    (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode "f=" 'tide-format)
    (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode "fr" 'tide-refactor)
    ;; rename
    (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode "rf" 'tide-rename-file)
    ;; errors
    (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode "ep" 'ls-explain-error-at-point)
    (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode "en" 'flycheck-next-error)
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
;; Use typescript-mode in TSX files
;;

(add-to-list 'auto-mode-alist '("\\.tsx$" . typescript-mode))
