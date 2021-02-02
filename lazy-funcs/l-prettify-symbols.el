;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; prettify-symbols related functions
;;

;;;###autoload
(defun add-symbols-to-mode (&optional symbols)
  (dolist (symbol (append generic-symbols symbols))
    (if (not-contains prettify-symbols-alist symbol)
        (push symbol prettify-symbols-alist))))
