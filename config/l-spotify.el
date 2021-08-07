;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; spotify related functions
;;


(bind-lazy-function 'spotify-menu-bar 'spotify-helper 'spotilau)
(bind-lazy-function 'spotify-menu 'spotify-status 'spotilau)

(spacemacs/set-leader-keys
  "a m s m" 'spotify-menu-bar
  "a m s a" 'spotify-menu)

;; set alias keybindings
(which-key-add-key-based-replacements "M-p" "Spotify")

(global-set-key (kbd "M-p M-p") 'spotify-menu-bar)
(global-set-key (kbd "M-p p") 'spotify-menu)





