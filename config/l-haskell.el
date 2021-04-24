;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; haskell related functions
;;

(eval-after-load "haskell"
  (lambda ()
    (load (join-path external-libs-dir "hs-lint/hs-lint.el"))
    (defun my-haskell-mode-hook ()
      (local-set-key "\C-cl" 'hs-lint))
    (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
    ))
