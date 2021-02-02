;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; tide related functions
;;


;;;###autoload
(defun tide-setup-hook ()
  (tide-setup)
  (eldoc-mode)
  (tide-hl-identifier-mode +1)
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-attr-value-indent-offset 2)
  (setq lsp-eslint-server-command '("node" (concat (getenv "HOME") "/var/src/vscode-eslint/server/out/eslintServer.js") "--stdio")))
