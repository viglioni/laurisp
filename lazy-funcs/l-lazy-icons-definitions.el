;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; icons-definitions related functions
;;

;;;###autoload
(defun icon-def (regex icon-name &optional face icon-pkg height v-adjust)
  (let ((f (or face 'all-the-icons-lpurple))
        (h (or height '1.0))
        (v (or v-adjust '0.0))
        (icon-pack (or icon-pkg 'all-the-icons-fileicon)))
    (list regex icon-pack icon-name :height h :v-adjust v :face f)))


