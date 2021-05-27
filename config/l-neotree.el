;;
;; @author Laura Viglioni
;; 2020~2021
;; GNU Public License 3.0
;;

;;
;; neotree related functions
;;


(with-eval-after-load "neotree"
  (setq neo-theme 'icons)
  (setq neo-window-fixed-size nil))

(with-eval-after-load "all-the-icons"
  (load-lib 'custom-icons-definitions)
  (setq all-the-icons-icon-alist custom-icons))


