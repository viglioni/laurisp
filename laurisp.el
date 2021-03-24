;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;


;;
;; Constants
;;
(setq functional-lib-dir "~/laurisp/personal-libs/functional")
(setq lazy-files-dir "~/laurisp/lazy-files")
(setq laurisp-core-dir "~/laurisp/core")

;;
;; add dirs to load path
;;
(add-to-load-path functional-lib-dir)
(add-to-load-path lazy-files-dir)
(add-to-load-path laurisp-core-dir)

;;
;; requires
;;
(require 'seq)



;;
;; global keys
;;

(global-set-key (kbd "<f19> <f19>") 'helm-M-x) 

;;
;; import all laurisp files
;;
(defun import-files (dir)
  (let* ((path (concat "~/laurisp/" dir))
         (all-files (directory-files path t "^l-[a-z\\-].*\\.el$"))
         (loaded-files (mapcar (lambda (laurisp-file)
                                 (load laurisp-file nil nil))
                               all-files)))
    (if (seq-reduce (lambda (acc val) (and acc val)) loaded-files t)
        "laurisp files loaded!"
      "an error has ocurred")))

(import-files "core")
(import-files "lazy-funcs")
(import-files "config")
(import-files "../.private")
(load "~/laurisp/external/emacs-grammarly/emacs-grammarly.el")

(provide 'laurisp)

