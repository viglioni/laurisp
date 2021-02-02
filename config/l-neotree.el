;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; neotree related functions
;;


(eval-after-load "neotree"
  (lambda ()
     (progn
       (setq neo-smart-open t)
       (setq neo-theme 'icons)
       (setq projectile-switch-project-action 'neotree-projectile-action)
       (setq neo-window-fixed-size nil)
       )))


