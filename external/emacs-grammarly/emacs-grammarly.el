;;
;; code from
;; https://github.com/mmagnus/emacs-grammarly/blob/master/emacs-grammarly.el
;; GPL-3.0
;;


;;; emacs-grammarly.el --- a simple plugin to sent text to Grammarly
;;; Commentary:
;;   URL: https://github.com/mmagnus/emacs-grammarly
;;; Code:
;;;###autoload
(defun grammarly-push ()
  "Save region to a tempfile and run Grammarly on it."
  (interactive)
  (kill-region (region-beginning) (region-end))
  ;;(insert "<<here>>")
  (call-process-shell-command "osascript ~/laurisp/external/emacs-grammarly/push.scpt")
  )
;;;###autoload
(defun grammarly-pull()
  "Save region to a tempfile and run Grammarly on it."
  (interactive)
  (call-process-shell-command "osascript ~/laurisp/external/emacs-grammarly/pull.scpt")
  (yank)
  )

(global-set-key (kbd "C-c C-g h") 'grammarly-push)
(global-set-key (kbd "C-c C-g l") 'grammarly-pull)

(provide 'emacs-grammarly)
;;; emacs-grammarly.el ends here
