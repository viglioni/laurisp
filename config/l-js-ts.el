;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;


;;
;; js/ts related functions
;;


;;
;; Definitions
;;

(setq js-indent-level 2)
(setq typescript-indent-level 2)
(bind-lazy-function 'import-module 'ls-import-js-libs 'laurascript)
(bind-lazy-function 'rename-this-file 'lsp-ts-rename-file 'launguage-server-protocol)
(bind-lazy-function 'explain-error-at-point 'lsp-explain-error-at-point 'launguage-server-protocol)
(bind-lazy-function 'find-ramda-docs 'open-ramda-docs 'ramda-docs)

;;
;; rjsx extra configs
;;

(add-to-list 'auto-mode-alist '("\\.js.*$" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))


;;
;; web-mode
;;

(with-eval-after-load "web-mode"
  ;;New prefixes for commands
  (spacemacs/declare-prefix-for-mode 'typescript-tsx-mode "me" "errors")
  (spacemacs/declare-prefix-for-mode 'typescript-tsx-mode "mf" "format")
  (spacemacs/declare-prefix-for-mode 'typescript-tsx-mode "mi" "HTML tags")
  (spacemacs/declare-prefix-for-mode 'typescript-tsx-mode "mb" "Fold/Unfold tags")
  (spacemacs/declare-prefix-for-mode 'typescript-tsx-mode "mn" "npm scripts")
  (spacemacs/set-leader-keys-for-major-mode 'typescript-tsx-mode
    ;;web mode feats
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
    "bf" 'web-mode-fold-or-unfold
    "bc" 'web-mode-element-children-fold-or-unfold
    "." 'spacemacs/web-transient-state/body
    ;;format
    "fo" 'lsp-organize-imports
    "fi" 'import-module
    "f=" 'lsp-format-buffer
    "==" 'lsp-format-buffer
    ;;rename
    "rf" 'rename-this-file
    ;;errors
    "ep" 'explain-error-at-point
    "en" 'flycheck-next-error
    ;;docs
    "dr" 'find-ramda-docs
    ;; npm
    ;; "nr" npm-choose-and-run
    ;; "no" npm-open-active-buffer
    ;; "nh" npm-hide-buffer
    ))

;;
;;typescript-mode
;;


(with-eval-after-load "typescript-mode"
  (load-lib 'ts-repl)
  ;;New prefixes for commands
  (spacemacs/declare-prefix-for-mode 'typescript-mode "me" "errors")
  (spacemacs/declare-prefix-for-mode 'typescript-mode "mf" "format")
  (spacemacs/declare-prefix-for-mode 'typescript-mode "mn" "npm scripts")
  (spacemacs/declare-prefix-for-mode 'typescript-mode "ms" "repl")
  (spacemacs/set-leader-keys-for-major-mode 'typescript-mode
    ;;format 
    "fo" 'lsp-organize-imports
    "fi" 'import-module
    "f=" 'lsp-format-buffer
    "==" 'lsp-format-buffer
    ;;rename
    "rf" 'rename-this-file
    ;;errors
    "ep" 'explain-error-at-point
    "en" 'flycheck-next-error
    ;; docs
    "dr" 'find-ramda-docs
    ;; npm
    ;; "nr" npm-choose-and-run
    ;; "no" npm-open-active-buffer
    ;; "nh" npm-hide-buffer
    ;; repl
    ;; "sc" 'run-ts
    ;; "se" 'ts-send-last-sexp
    ;; "sb" 'ts-send-buffer
    "sb" 'ts-repl-exec-ts-buffer
    "se" 'ts-repl-send-last-sexp
    "C-x C-e" 'ts-repl-send-last-sexp
    ))
