;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;


;;
;; Constants
;;
(setq personal-lib-dir "~/laurisp/personal-libs/")
(setq external-libs-dir "~/laurisp/external")
(setq laurisp-config-dir "~/laurisp/config")
(setq private-files-dir "~/private-files/emacs-files")
(setq lazy-files-dir "~/laurisp/lazy-files")

;;
;; add lib dirs to load path
;;

(let ((default-directory  personal-lib-dir))
  (normal-top-level-add-subdirs-to-load-path))

(let ((default-directory external-libs-dir))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path private-files-dir)
(add-to-list 'load-path lazy-files-dir)

;;
;; requires
;;

(require 'functional)
(require 'laurisp-core)



;;
;; import files functions 
;;

;;;###autoload
(defun import-laurisp-files (dir)
  "import all laurisp files from a dir"
  (let* ((path (concat "~/laurisp/" dir))
         (all-files (directory-files path t "^l-[a-z\\-].*\\.el$"))
         (loaded-files (mapcar (lambda (laurisp-file)
                                 (load laurisp-file nil nil))
                               all-files)))
    (if (seq-reduce (lambda (acc val) (and acc val)) loaded-files t)
        "laurisp files loaded!"
      "an error has ocurred")))

;;;###autoload
(defmacro load-lib (lib-name)
  "requires a lib in external or personal lib dir. Usage example:
   (load-lib 'emacs-grammarly)"
  `(require ,lib-name))

;;;###autoloading
(defmacro bind-lazy-function (func-name lib-func-name package-name)
  "Creates an interactive of a lib that is not imported by default
   that loads it when is called
   Usage example:
   (bind-lazy-function 'spotify-func 'spotify-status 'spotilau)
   (global-set-key (kbd \"M-p M-p\") 'spotify-func)"
  `(defun ,(eval func-name) ()
     (interactive)
     (load-lib ,package-name)
     (call-interactively ,lib-func-name)))


;;;###autoload
(defmacro call-lazy-function (func-name package-name)
  "Calls a function from a lib that is not imported by default"
  `(progn
     (load-lib ,package-name)
     (call-interactively ,func-name)))


;;
;; Importing files
;;

(import-laurisp-files "config")

(provide 'laurisp)

