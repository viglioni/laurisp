;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; string related functions
;;

;;;###autoload
(defun join-path (path file)
  "concat path and file. Adds '/' to the end of the path if necessary"
  (concat path (if (string-match-p "/$" path) "" "/") file))

;;;###autoload
(defun file-extension (filename extension)
  "returns filename.extension"
  (concat filename "." extension))

;;;###autoload
(defun regex-matches (regexp string &optional pos matches)
  "Returns a list of matches"
  (let* ((actual-pos (or pos 0))
         (m-pos (string-match regexp string actual-pos))
         (match (and m-pos (match-string 0 string))))
    (if match
        (regex-matches regexp string (match-end 0) (append matches (list match)))
      matches)))


;;;###autoload
(defun get-string-from-file (filePath)
  "Return filePath's file content in a string"
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

;;;###autoload
(defun string-from-file (file-path)
  "Return file-path's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))
