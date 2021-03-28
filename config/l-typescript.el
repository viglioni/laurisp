;; 
;;  @author Laura Viglioni
;;  2020
;;  GNU Public License 3.0
;; 


;; 
;;  typescript related functions
;; 


(setq js-indent-level 2)
(setq typescript-indent-level 2)



;; 
;;  rjsx extra configs
;; 

(add-to-list 'auto-mode-alist '("\\.js.*$" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))


;;
;; web-mode 
;;

(eval-after-load 'web-mode
  (lambda ()
    ;; New prefixes for commands
    (spacemacs/declare-prefix-for-mode 'typescript-tsx-mode "me" "errors")
    (spacemacs/declare-prefix-for-mode 'typescript-tsx-mode "mf" "format")
    (spacemacs/declare-prefix-for-mode 'typescript-tsx-mode "mi" "HTML tags")
    (spacemacs/declare-prefix-for-mode 'typescript-tsx-mode "mb" "Fold/Unfold tags")
    (spacemacs/set-leader-keys-for-major-mode 'typescript-tsx-mode
      ;; web mode feats
      "ii" 'web-mode-element-insert-at-point
      "iv" 'web-mode-element-vanish
      "ik" 'web-mode-element-kill
      "is" 'web-mode-element-select
      "iw" 'web-mode-element-wrap
      "ir" 'web-mode-element-rename
      "ic" 'web-mode-element-clone
      "i/" 'web-mode-element-close
      "ib" 'web-mode-element-beginning
      "ie" 'web-mode-element-end
      "n" 'web-mode-element-next
      "p" 'web-mode-element-previous
      "bf" 'web-mode-fold-or-unfold
      "bc" 'web-mode-element-children-fold-or-unfold
      "." 'spacemacs/web-transient-state/body
      ;; format 
      "ff" 'lsp-execute-code-action
      "fo" 'lsp-organize-imports
      "fi" 'ls-import-js-libs
      "f=" 'lsp-format-buffer
      "==" 'lsp-format-buffer
      ;; rename
      "rf" 'lsp-ts-rename-file
      ;; errors
      "ep" 'lsp-explain-error-at-point
      "en" 'flycheck-next-error
      )))

;;
;; typescript-mode
;;

(eval-after-load 'typescript-mode
  (lambda ()
    ;; New prefixes for commands
    (spacemacs/declare-prefix-for-mode 'typescript-mode "me" "errors")
    (spacemacs/declare-prefix-for-mode 'typescript-mode "mf" "format")
    (spacemacs/set-leader-keys-for-major-mode 'typescript-mode
      ;; format 
      "ff" 'lsp-execute-code-action
      "fo" 'lsp-organize-imports
      "fi" 'ls-import-js-libs
      "f=" 'lsp-format-buffer
      "==" 'lsp-format-buffer
      ;; rename
      "rf" 'lsp-ts-rename-file
      ;; errors
      "ep" 'lsp-explain-error-at-point
      "en" 'flycheck-next-error
      )))
