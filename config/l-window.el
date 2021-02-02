;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; window related functions
;;

;; walk through windows
(global-set-key (kbd "C-x <up>") 'evil-window-up)
(global-set-key (kbd "C-x <down>") 'evil-window-down)
(global-set-key (kbd "C-x <left>") 'evil-window-left)
(global-set-key (kbd "C-x <right>") 'evil-window-right)

;; resize windows
(global-set-key (kbd "C-c C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-c C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-c C-<up>") 'enlarge-window)
(global-set-key (kbd "C-c C-<down>") 'shrink-window)
