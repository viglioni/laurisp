;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;


;;
;; Files related functions
;;


;;
;; inserting on file
;;

;;;###autoload
(defun remove-suffix (file-name)
  "remove suffix of file-name"
  (replace-regexp-in-string "\\.[a-z]*$" ""  file-name))

;;;###autoload
(defun insert-on-fst-empty-line (text current-point)
  "inserts text on the first empty line of the buffer"
  (progn
    (beginning-of-buffer)
    (search-forward-regexp "^$")
    (insert text)
    (goto-char (+ current-point (length text)))
    t))


;;;###autoload
;; (defun relative-path (file1 file2)
;;   "TEMPORARIALLY DEPRECATED"
;;   (let* ((path1-list (split-string (file-truename file1) "/"))
;;          (path2-list (split-string (file-truename file2) "/"))
;;          (zipped-path (zip path1-list path2-list))
;;          (diff-path (seq-filter (lambda (pair) "" (not (equal (head pair) (head (tail pair)))))  zipped-path))
;;          (unzipped (unzip diff-path))
;;          (pre-path1  (nbutlast (seq-filter #'bool (head unzipped)) 1))
;;          (pre-path2  (seq-filter #'bool (head (tail unzipped))))
;;          (path1 (concat "./" (s-join "/" (seq-map (lambda (el) "" "..") pre-path1))))
;;          (filename (head (last pre-path2)))
;;          (path2 (concat (s-join "/" pre-path2)))
;;          (relative-path (join-path path1 path2)))
;;     relative-path))

