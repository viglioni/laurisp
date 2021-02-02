;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; dotfiles related functions
;;

;; open dotfiles with sh-mode
(add-to-list 'auto-mode-alist '("/\\.[a-zA-Z0-09]*rc$" . sh-mode))
(add-to-list 'auto-mode-alist '("/\[a-zA-Z0-09]*rc$" . sh-mode))
