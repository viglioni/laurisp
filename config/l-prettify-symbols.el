;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; prettify-symbols related functions
;;

;; set to run globally


(global-prettify-symbols-mode +1)

;; symbols do be used in any mode
(setq generic-symbols '(("->" . ?→)
                        ("=>" . ?⇒)
                        ("/=" . ?≠)
                        ("!==" . ?≠)
                        ("<=" . ?≤)
                        (">=" . ?≥)
                        ))


;; applying to elisp mode
(add-hook 'emacs-lisp-mode-hook
          (lambda () (add-symbols-to-mode)))


