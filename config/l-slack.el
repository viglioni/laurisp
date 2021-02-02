;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; slack related functions
;;




(eval-after-load "slack"
  (lambda ()
    (progn
      (add-hook 'slack-mode-hook 'emojify-mode)
      (load-file "~/.private/slack-config.el")
      )))



