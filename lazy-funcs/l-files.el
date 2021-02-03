;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; files related functions
;;

;;
;; opening files
;;

;;;###autoload
(defun open-spacemacs-config ()
  "opens .spacemacs file"
  (interactive)
  (find-file "~/.spacemacs"))

;;;###autoload
(defun open-skhd-config ()
  "opens .skhdrc file"
  (interactive)
  (find-file "~/../.skhdrc"))

;;;###autoload
(defun open-yabai-config ()
  "opens .yabairc file"
  (interactive)
  (find-file "~/../.yabairc"))

;;;###autoload
(defun open-zsh-config ()
  "opens .zshrc file"
  (interactive)
  (find-file "~/../.zshrc"))

;;;###autoload
(defun open-laurisp ()
  "opens laurisp.el file"
  (interactive)
  (find-file "~/laurisp/laurisp.el"))

;;
;; creating files
;;

;;;###autoload
(defun new-laurisp-file (name dir)
  "create laurisp file"
  (interactive "sInsert filename: \nDWhere? ")
  (let* ((filename (concat "l-" name ".el"))
         (file (touch filename dir)))
    (echo-into file (format ";;\n;; @author Laura Viglioni\n;; 2021\n;; GNU Public License 3.0\n;;\n\n;;\n;; %s related functions\n;;\n\n" name))
    (find-file file)))

