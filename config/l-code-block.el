;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; code-block related functions
;;


(which-key-add-key-based-replacements "C-c b" "code-blocks")

(global-set-key (kbd "C-c b t") 'hs-toggle-hiding)
(global-set-key (kbd "C-c b l") 'hs-hide-level)
(global-set-key (kbd "C-c b H") 'hs-hide-all)
(global-set-key (kbd "C-c b S") 'hs-show-all)
(global-set-key (kbd "C-c b s") 'hs-show-block)
(global-set-key (kbd "C-c b h") 'hs-hide-block)
