;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; spotify related functions
;;


(setq spotify-menu-bar (bind-lazy-function 'spotify-helper 'spotilau))
(setq spotify-menu (bind-lazy-function 'spotify-status 'spotilau))

(spacemacs/set-leader-keys
  "a m s m" spotify-menu-bar
  "a m s a" spotify-menu)

;; set alias keybindings
(which-key-add-key-based-replacements "M-p" "Spotify")

(global-set-key (kbd "M-p M-p") spotify-menu-bar)
(global-set-key (kbd "M-p p") spotify-menu)





