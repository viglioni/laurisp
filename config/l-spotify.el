;; -*- lexical-binding: t -*-
;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; spotify related functions
;;



;; set alias keybindings
(which-key-add-key-based-replacements "M-p" "Spotify")
(global-set-key (kbd "M-p M-p") 'spotify-helper)
(global-set-key (kbd "M-p p") 'spotify-status )





