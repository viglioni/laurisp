;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;


;;
;; Files related functions
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
;; inserting on file
;;


;;;###autoload
(defun remove-suffix (file)
  "remove suffix of file"
  (replace-regexp-in-string "\\.[a-z]*$" ""  file))

;;;###autoload
(defun insert-text-on-first-empty-line (text current-point)
  ""
  (progn
    (beginning-of-buffer)
    (search-forward-regexp "^$")
    (insert text)
    (goto-char (+ current-point (length text)))
    t))

;;;###autoload
(defun new-laurisp-file (name dir)
  "create laurisp file"
  (interactive "sInsert filename: \nDWhere? ")
  (let* ((filename (concat "l-" name ".el"))
         (file (touch filename dir)))
    (echo-into file (format ";;\n;; @author Laura Viglioni\n;; 2021\n;; GNU Public License 3.0\n;;\n\n;;\n;; %s related functions\n;;\n\n" name))
    (find-file file)))

;;;###autoload
(defun relative-path (file1 file2)
  ""
  (let* ((path1-list (split-string (file-truename file1) "/"))
         (path2-list (split-string (file-truename file2) "/"))
         (zipped-path (zip path1-list path2-list))
         (diff-path (seq-filter (lambda (pair) "" (not (equal (head pair) (head (tail pair)))))  zipped-path))
         (unzipped (unzip diff-path))
         (pre-path1  (nbutlast (seq-filter #'bool (head unzipped)) 1))
         (pre-path2  (seq-filter #'bool (head (tail unzipped))))
         (path1 (concat "./" (s-join "/" (seq-map (lambda (el) "" "..") pre-path1))))
         (filename (head (last pre-path2)))
         (path2 (concat (s-join "/" pre-path2)))
         (relative-path (join-path path1 path2)))
    relative-path))

