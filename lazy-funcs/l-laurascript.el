;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; tide related functions
;;

(require 'helm)
(require 'l-functional)
(require 'l-string)
(require 'l-general)

;;
;; Tide setup
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
  (setq lsp-eslint-server-command '("node" (concat (getenv "HOME") "/var/src/vscode-eslint/server/out/eslintServer.js") "--stdio"))
  )

;;
;; Import JS libs from package.json
;;

;;;###autoload
(defun laurascript--get-project-libs ()
  "get all libs from package.json dependencies, it searches from
   this file in project root or current dir"
  (let ((pjson-file (join-path (or (projectile-project-root) ".") "package.json")))
    (throw-if (not (file-exists-p pjson-file)) "package.json not found!")
    (-> pjson-file
        ((get-string-from-file)
         (regex-matches (rx "\"dependencies"  (+ (not "}")) "}"))
         (head)
         (replace-regexp-in-string (rx "\"dependencies\": {") "")
         (regex-matches (rx line-start (+ (not ":")) ":"))
         (mapcar (curry replace-regexp-in-string (rx (or "\"" ":" " " "" "\n")) ""))
         (funcall (lambda (lst) (sort lst #'string<)))))))

;;;###autoload
(defun laurascript--insert-import (lib-name import-name)
  "Inserts 'import $import-name from $lib-name' on the first
   empty line of buffer"
  (insert-on-fst-empty-line
   (concat "import " import-name " from \"" lib-name "\"\n")
   (point)))

;;;###autoload
(defun laurascript--helm-import-lib (lib-name)
  (helm
   :prompt "Import lib as: "
   :volatile t
   :sources (helm-build-dummy-source "Import name source"
              :action (curry laurascript--insert-import lib-name))))

;;;###autoload
(defun laurascript--helm-libs-source ()
  (helm-build-sync-source "Libs Source"
    :volatile t
    :multiline nil
    :candidates (laurascript--get-project-libs)
    :action 'laurascript--helm-import-lib ))

;;;###autoload
(defun ls-import-js-libs ()
  "Add 'import $import-name from $lib-name' on the first empty line.
   It searches on project root directory or current directory for 
   package.json and searches for all libs inside its dependencies"
  (interactive)
  (helm :prompt "Choose a lib to import: "
        :sources (laurascript--helm-libs-source)
        :buffer "*helm buffer source*"))


;;
;; explain error at point
;;
(setq laurascript--error-buffer "laurascript-error-at-point")

;;;###autoload
(defun laurascript--kill-error-buffer (key)
  (interactive "k")
  (if (get-buffer laurascript--error-buffer)
      (kill-buffer laurascript--error-buffer)))

;;;###autoload
(defun laurascript--config-error-buffer (msg)
  (let* ((max-text-width 100)
         (total-margin (- (window-text-width) max-text-width))
         (margin-lateral  (/ total-margin 2))
         (min-lines (+ 4 (ceiling (/ (length msg) (min max-text-width (window-text-width))))))
         (actual-height (window-body-height)))
    (if (> total-margin 0)
        (progn (setq left-margin-width margin-lateral)
               (setq right-margin-width margin-lateral)
               (set-window-buffer (selected-window) (current-buffer))))
    (enlarge-window (- min-lines actual-height))
    (read-only-mode)
    (visual-line-mode)))

;;;###autoload
(defun ls-explain-error-at-point ()
  (interactive)
  (let ((err (flycheck-overlay-errors-at (point))))
    (if err
        (let* ((msg (head (mapcar 'flycheck-error-message err)))
               (buff-name (get-buffer-create laurascript--error-buffer))
               (error-buff (get-buffer buff-name)))
          (display-buffer-in-side-window error-buff '((side . bottom)))
          (switch-to-buffer-other-window error-buff)
          (erase-buffer)
          (insert (concat "\n" msg))
          (laurascript--config-error-buffer msg)
          (call-interactively 'laurascript--kill-error-buffer)))
    (error "No error at point"))) 






