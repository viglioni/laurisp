(defun load-file (file-name)
  "search for file and loads it"
  (mapcar
   'load
   (directory-files-recursively
    "."
    (concat file-name ".el"))))
