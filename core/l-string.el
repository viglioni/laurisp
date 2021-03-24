;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; string related functions
;;

(require 'functional)
(require 'l-general)

;;;###autoload
(defun join-path (path filename)
  "concat path and file. Adds '/' to the end of the path if necessary"
  (throw-if (any-nil? path filename) "path or filename is nil")
  (concat path (if (string-match-p "/$" path) "" "/") filename))

;;;###autoload
(defun file-extension (filename extension)
  "returns filename.extension"
  (throw-if (any-nil? filename extension) "filename or extension is nil")
  (concat filename "." extension))

;;;###autoload
(defun regex-matches (regexp string &optional pos matches)
  "Returns a list of matches"
  (throw-if (any-nil? regexp string) "regexp or string is nil")
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      matches)))


;;;###autoload
(defun get-string-from-file (filepath)
  "Return filepath's file content in a string"
  (throw-if (any-nil? filepath) "filepath is nil")
  (throw-if (not (file-exists-p filepath)) "file does not exists")
  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string)))

;;;###autoload
(defun remove-suffix (file-name)
  "remove suffix of file-name"
  (throw-if (any-nil? file-name) "file-name is nil")
  (replace-regexp-in-string "\\.[a-z]*$" ""  file-name))

;;;###autoload
(defun go-to-fst-empty-line ()
  "search the first empty line in buffer and go to it"
  (beginning-of-buffer) ;; TODO  use ‘(goto-char (point-min))’ instead.
  (search-forward-regexp "^$"))

;;;###autoload
(defun insert-on-fst-empty-line (text current-pos)
  "inserts text on the first empty line of the buffer and
    return the cursor to its position"
  (throw-if (any-nil? text current-pos) "text or current-pos is nil")
  (let* ((empty-line-pos (progn (go-to-fst-empty-line) (point)))
         (is-after? (> current-pos empty-line-pos))
         (return-pos (if is-after?
                         (+ current-pos (length text))
                       current-pos)))
    (insert text)
    (goto-char return-pos)
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

(provide 'l-string)
