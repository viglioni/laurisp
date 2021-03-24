;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;


;;;###autoload
(defun load-test-file (file-name)
  "search for file and loads it"
  (mapcar 'load
          (directory-files-recursively "." (concat file-name ".el"))))

;;;###autoload
(defmacro test-suite (description &rest body)
  "the same as describe, but with defun indentation"
  (declare (indent defun))
  `(describe ,description
             ,@body))

;;;###autoload
(defmacro context (description &rest body)
  "the same as describe, but more idiomatic and with defun indentation"
  (declare (indent defun))
  `(describe (concat "when " ,description)
             ,@body))

;;;###autoload
(defmacro it-should (description &rest body)
  "the same as it, but with defun indentation"
  (declare (indent defun))
  `(it (concat "should " ,description)
       ,@body))

(provide 'buttercup-helpers)
