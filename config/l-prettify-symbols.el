;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; prettify-symbols related functions
;;

;; set to run globally


;; symbols do be used in any mode
(setq generic-symbols
      '(("->" . ?→)
        ("=>" . ?⇒)
        ("/=" . ?≠)
        ("!==" . ?≠)
        ("<=" . ?≤)
        (">=" . ?≥)
        ))


(with-eval-after-load "prog-mode"

  (defun add-symbols-to-mode (&optional symbols)
    (dolist symbols
      (if (not-contains? prettify-symbols-alist symbol)
          (push symbol prettify-symbols-alist))))

  (global-prettify-symbols-mode +1)

  ;; applying to all modes
  (add-hook 'prog-mode-hook
            (lambda () (add-symbols-to-mode generic-symbols))))



