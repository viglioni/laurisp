;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;


;;
;; Constants
;;
(setq personal-libs-dir "~/laurisp/personal-libs")
(setq lazy-files-dir "~/laurisp/lazy-files")
(setq laurisp-core-dir "~/laurisp/core")

;;
;; add dirs to load path
;;
(add-to-load-path personal-libs-dir)
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

(defun import-compiled-files (dir)
  (let* ((path (concat "~/laurisp/" dir))
         (all-files (directory-files path t "^l-[a-z\\-].*\\.elc"))
         (loaded-files (mapcar (lambda (laurisp-file)
                                 (load laurisp-file nil nil))
                               all-files)))
    (if (seq-reduce (lambda (acc val) (and acc val)) loaded-files t)
        "laurisp files loaded!"
      "an error has ocurred")))


(import-compiled-files "core")
(import-compiled-files "lazy-funcs")
(import-compiled-files "config")
(import-compiled-files "../.private")
(load "~/laurisp/external/emacs-grammarly/emacs-grammarly.elc")

(provide 'laurisp)

