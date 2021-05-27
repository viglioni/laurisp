;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; slack related functions
;;

(with-eval-after-load "slack"
  (add-hook 'slack-mode-hook 'emojify-mode)
  (load-lib 'slack-private))



