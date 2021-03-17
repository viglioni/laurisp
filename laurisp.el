;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

(require 'seq)

;;
;; Constants
;;

(setq lazy-files-dir "~/laurisp/lazy-files")

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
                                 (load laurisp-file))
                               all-files)))
    (if (seq-reduce (lambda (acc val) (and acc val)) loaded-files t)
        "laurisp files loaded!"
      "an error has ocurred")))

(import-files "core")
(import-files "lazy-funcs")
(import-files "config")
(import-files "test")
(import-files "../.private")
(load "~/laurisp/external/emacs-grammarly/emacs-grammarly.el")

(provide 'laurisp)

