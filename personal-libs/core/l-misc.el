;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; misc related functions
;;

;;(require 'functional)

;;;###autoload
(defun create-laurisp-core ()
  (interactive)
  (let* ((filename "laurisp-core.el")
         (files (directory-files "." t "^l-[a-z\\-].*\\.el$"))
         (content (fp/pipe files
                     ((mapcar 'get-string-from-file)
                      (string-join)
                      (replace-regexp-in-string "(provide '[a-z\-]+)" "")))))
    (with-temp-buffer
      (insert content)
      (insert "(provide 'laurisp-core)")
      (write-file filename))
    (byte-compile-file filename)))

;;;###autoload
;; (defun recompile-laurisp ()
;;   (interactive)
;;   (seq-do
;;    (lambda (dir)
;;      (byte-recompile-directory (concat "~/laurisp/" dir) 0 t))
;;    ["lazy-files" "lazy-funcs" "personal-libs" "core" "external"]))

