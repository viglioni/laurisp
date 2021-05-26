;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; lsp related functions
;;

;;
;; explain error at point
;;

(setq LauSP--error-buffer "LauSP-error-at-point")

;;;###autoload
(defun LauSP--kill-error-buffer (key)
  (interactive "k")
  (if (get-buffer LauSP--error-buffer)
      (kill-buffer LauSP--error-buffer)))

;;;###autoload
(defun LauSP--config-error-buffer (msg)
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
(defun lsp-explain-error-at-point ()
  (interactive)
  (let ((err (flycheck-overlay-errors-at (point))))
    (if err
        (let* ((msg (head (mapcar 'flycheck-error-message err)))
               (buff-name (get-buffer-create LauSP--error-buffer))
               (error-buff (get-buffer buff-name)))
          (display-buffer-in-side-window error-buff '((side . bottom)))
          (switch-to-buffer-other-window error-buff)
          (erase-buffer)
          (insert (concat "\n" msg))
          (LauSP--config-error-buffer msg)
          (call-interactively 'LauSP--kill-error-buffer)))
    (error "No error at point"))) 


;;
;; Typescript/Javascript
;;

;;;###autoload
(defun lsp-ts-rename-file ()
  "Rename current file and all it's references in other files."
  (interactive)
  (let* ((name (buffer-name))
         (old (buffer-file-name))
         (basename (file-name-nondirectory old)))
    (unless (and old (file-exists-p old))
      (error "Buffer '%s' is not visiting a file." name))
    (let ((new (read-file-name "New name: " (file-name-directory old) basename nil basename)))
      (when (get-file-buffer new)
        (error "A buffer named '%s' already exists." new))
      (when (file-exists-p new)
        (error "A file named '%s' already exists." new))
      (lsp--send-execute-command
       "_typescript.applyRenameFile"
       (vector (list :sourceUri (lsp--buffer-uri)
                     :targetUri (lsp--path-to-uri new))))
      (mkdir (file-name-directory new) t)
      (rename-file old new)
      (rename-buffer new)
      (set-visited-file-name new)
      (set-buffer-modified-p nil)
      (lsp-disconnect)
      (setq-local lsp-buffer-uri nil)
      (lsp)
      (lsp--info "Renamed '%s' to '%s'." name (file-name-nondirectory new)))))


(provide 'launguage-server-protocol)


;;
;; compilation warnings
;;


;; launguage-server-protocol.el:14:7:Warning: assignment to free variable
;; ‘LauSP--error-buffer’

;; In LauSP--kill-error-buffer:
;; launguage-server-protocol.el:19:19:Warning: reference to free variable
;; ‘LauSP--error-buffer’

;; In lsp-explain-error-at-point:
;; launguage-server-protocol.el:43:46:Warning: reference to free variable
;; ‘LauSP--error-buffer’

;; In lsp-ts-rename-file:
;; launguage-server-protocol.el:82:19:Warning: assignment to free variable
;; ‘lsp-buffer-uri’

;; In end of data:
;; launguage-server-protocol.el:88:1:Warning: the following functions are not known to be defined:
;; lsp--send-execute-command, lsp--buffer-uri, lsp--path-to-uri,
;; lsp-disconnect, lsp--info
