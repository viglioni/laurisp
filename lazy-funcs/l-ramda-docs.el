;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;


(require 'request)
(require 'l-general)
(require 'functional)
(require 'l-string)

;;
;; ramda-docs related functions
;;

(defvar ramda-html nil)
(setq ramda-docs-url "https://ramdajs.com/docs/")

;;;###autoload
(defun download-ramda-html ()
  (unless ramda-html
    (request ramda-docs-url
      :sync t
      :success (cl-function
                (lambda (&key data &allow-other-keys) (setq ramda-html data)))
      :error  (cl-function
               (lambda (&key error-thrown &allow-other-keys&rest _)
                 (message "Got error: %S" error-thrown))))))

;;;###autoload
(defun function-info (html-line)
  (let* ((filtered-string
          (-> html-line
             ((replace-regexp-in-string
               (rx (or "data-name" "data-category" "=" "\"" ">"))
               "")
              ((lambda (str) (split-string str " "))))))
         (fn-name (head filtered-string))
         (fn-category (nth 1 filtered-string)))
    (cons (format "(%s) %s" fn-category fn-name) fn-name)))

;;;###autoload
(defun parse-funcs-html (html)
  (regex-matches "data-name=\"[[:alpha:]_]*\" data-category=\"[[:alpha:]]*\"" html))

;;;###autoload
(defun helm-ramda-candidates ()
  (throw-if (any-nil? ramda-html) "ramda's page wasn't downloaded!")
  (-> ramda-html
     ((parse-funcs-html)
      (mapcar 'function-info)
      (alist-sort-by-cdr-ci))))

;;;###autoload
(defun open-ramda-doc-url (fn-name)
  (browse-url (concat ramda-docs-url "#" fn-name)))

;;;###autoload
(defun open-ramda-docs ()
  (interactive)
  (download-ramda-html)
  (helm :prompt "Choose function: "
        :sources (helm-build-sync-source "ramda functions"
                   :candidates 'helm-ramda-candidates
                   :action 'open-ramda-doc-url)))

