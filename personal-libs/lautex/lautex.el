;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; org-latex-pdf related functions
;;

(require 'helm)
(message "loading lautex...")

;;
;; latex preview
;;


;;;###autoload
(defun LauTeX-preview-latex-on-buffer ()
  (interactive)
  (latex-define-preview-settings)
  (org-clear-latex-preview)
  (org-latex-preview '(16)))

;;;###autoload
(defun LauTeX-preview-latex-on-section ()
  (interactive)
  (latex-define-preview-settings)
  (org-latex-preview '(4))
  (org-latex-preview))


;;
;; common functions
;;

;;;###autoload
(defun LauTex-compile-org-to-pdf ()
  (interactive)
  (if (and (boundp 'org-beamer-mode) org-beamer-mode)
      (org-beamer-export-to-pdf)
    (org-latex-export-to-pdf)))


;;;###autoload
(defun lautex--get-org-buffer-name (src-buffer)
  (replace-regexp-in-string "\\[" "" (nth 2 (split-string src-buffer " "))))

;;;###autoload
(defun lautex--get-org-file-name ()
  "If in a edit-special buffer, return the org one,
   else return the buffer it was called"
  (or (buffer-file-name)
      (fp/pipe (buffer-name)
         ((lautex--get-org-buffer-name)
          (get-buffer)
          (buffer-file-name)))))

;;;###autoload
(defun lautex--get-text (regexp-prefix regexp-suffix str)
  "returns string between two regex prefixes"
  (fp/pipe str
     ((replace-regexp-in-string regexp-prefix "")
      (replace-regexp-in-string regexp-suffix ""))))

;;;###autoload
(defun lautex--command (command-name arg)
  "return string \\command-name{arg}"
  (concat "\\" command-name "{" arg "}"))

;;;###autoload
(defun lautex--insert-command (command-name arg)
  "insert \\command-name{arg}"
  (insert (lautex--command command-name arg))
  (delete-horizontal-space))

;;
;;  inserting LaTeX enviroment in org mode
;;  and editing them in special mode
;;

(defvar lautex--env-names
  (sort '("text" "theorem" "definition" "remark" "example" "proposition" "figure" "Table"  "description" "enumerate" "itemize" "list"  "math" "displaymath" "split" "array" "eqnarray" "equation" "equation*" "Matrix" "environments" "Cases" "align" "align*" "alignat" "environments" "center" "flushleft" "flushright" "minipage" "quotation" "quote" "verbatim" "verse" "tabbing" "tabular" "Thebibliography" "Titlepage") 'string<))


;;;###autoload
(defun lautex--insert-env (env-name)
  "inserts LaTeX environment"
  (insert (concat
           (lautex--command "begin" env-name)
           "\n\n"
           (lautex--command "end" env-name))))

(setq lautex--helm-env-sources
      (helm-build-sync-source "Environment"
          :candidates 'lautex--env-names
          :action 'lautex--insert-env))

(setq lautex--helm-env-sources-fallback
      (helm-build-dummy-source "Environment"
        :action 'lautex--insert-env))

;;;###autoload
(defun LauTeX-org-env ()
  "inserts LaTeX env and opens edit special"
 (interactive)
  (if (eq 'org-mode major-mode)
      (progn
        (helm
         :history t
         :volatile nil
         :sources '(lautex--helm-env-sources lautex--helm-env-sources-fallback))
        (org-edit-special)
        (previous-line)
        (spacemacs/indent-region-or-buffer))))

;;;###autoload
(defun LauTeX-org-env-exit ()
  "exits edit special and toggle latex fragment to image"
  (interactive)
  (org-edit-src-exit)
  (org-clear-latex-preview (point) (inc (point)))
  (org-toggle-latex-fragment))

;;;###autoload
(defun LauTeX-preview-org-env ()
  "preview on org buffer latex changes in special edit buffer"
  (interactive)
  (let* ((edit-buff (buffer-name))
         (original-buf (lautex--get-org-buffer-name edit-buff))
         (buffer-exists (bool (get-buffer original-buf))))
    (if buffer-exists
        (progn
          (org-edit-src-save)
          (switch-to-buffer original-buf)
          (org-clear-latex-preview (point) (+ 1 (point)))
          (org-toggle-latex-fragment)
          (switch-to-buffer edit-buff))
      (print (concat "Buffer " original-buf " does not exists!")))))


;;
;; Reference existing labels
;;
(defvar lautex--regex-prefix-label  ".*label\{" "regex prefix for \\label{...}")

(defvar lautex--regex-sufix-label  "\}.*" "regex sufix for \\label{...}")

;;;###autoload
(defun lautex--get-label (line)
  "get string %s in .*\\label{%s}.*"
  (fp/pipe line ((lautex--get-text
             lautex--regex-prefix-label
             lautex--regex-sufix-label)
           (replace-regexp-in-string ":CUSTOM_ID: " ""))))

;;;###autoload
(defun lautex--insert-reference (label)
  "insert \\ref{label} on text"
  (let ((label-type (capitalize (lautex--label-type label))))
    (unless (string-empty-p label-type)
      (insert (concat label-type " "))))
  (lautex--insert-command "ref" label))

;;;###autoload
(defun lautex--label-type (label)
  (let* ((org-label (replace-regexp-in-string ":CUSTOM_ID: " "" label))
         (splitted (split-string org-label ":"))
         (label-type (if (= 2 (length splitted)) (head splitted) "")))
    (upcase label-type)))

;;;###autoload
(defun lautex--build-reference-candidate (match)
  (let* ((label (lautex--get-label match))
         (description (fp/pipe label
                         ((replace-regexp-in-string ":CUSTOM_ID: " "" label)
                          (replace-regexp-in-string "[\\._-]" " ")
                          (replace-regexp-in-string ".*:" "")
                          (capitalize))))
         (label-type (lautex--label-type label))
         (full-description (if (string-empty-p label-type)
                               description
                             (string-join (list label-type description) ": "))))
    (cons full-description label)))


;;;###autoload
(defun lautex--reference-candidates ()
  "Get all labels from a org/latex file"
  (let* ((text (get-string-from-file  (lautex--get-org-file-name)))
         (matches (regex-matches (rx (| (and "label{" (* nonl) "}")
                                        (and ":CUSTOM_ID:" (? space) (* nonl))))
                                 text)))
    (seq-map 'lautex--build-reference-candidate matches)))

;;;###autoload
(defun lautex--helm-label-source ()
  (helm-build-sync-source "Label Source"
    :candidates (lautex--reference-candidates)
    :action 'lautex--insert-reference))

;;;###autoload
(defun LauTeX-insert-reference ()
  (interactive)
  (helm :sources (lautex--helm-label-source)
        :buffer "*helm buffer source*"))


;;
;; Insert libs
;;
(defvar lautex--regex-bib-files "\\.bib$" "regex to match .bib files")

;;;###autoload
(defun lautex--insert-citation (citation)
  "insert \\ref{citation} on text"
  (lautex--insert-command "cite" citation))

;;;###autoload
(defun lautex--get-bib-files ()
  "get all .bib files in your project from your project root
   or where this function is called"
  (directory-files-recursively
   (or (projectile-project-root) default-directory)
   lautex--regex-bib-files))

;;;###autoload
(defun lautex--get-citation (line)
  "get string %s in .*\\label{%s}.*"
  (fp/pipe line
     ((replace-regexp-in-string "@[a-zA-Z]*\{" "")
      (replace-regexp-in-string "," "" ))))

(defun lautex--bib-citations (file-path)
  "get all citations from a given file"
  (fp/pipe (get-string-from-file file-path)
     ((regex-matches "@[a-zA-Z]*\{[a-z0-9A-Z].*,")
      (seq-map 'lautex--get-citation))))

;;;###autoload
(defun lautex--bib-info (line)
  "get info from a bib line"
  (fp/pipe line
     ((replace-regexp-in-string "[a-z]* ?= ?\{*" "")
      (replace-regexp-in-string "\}.*" ""))))

;;;###autoload
(defun lautex--bib-titles (file)
  (let* ((file-content (get-string-from-file file))
         (titles (regex-matches "title ?=.*" file-content)))
    (seq-map 'lautex--bib-info titles)))


;;;###autoload
(defun lautex--bib-authors (file)
  (let* ((file-content (get-string-from-file file))
         (authors (regex-matches "author ?=.*" file-content)))
    (seq-map 'lautex--bib-info authors)))


;;;###autoload
(defun lautex--bib-year (file)
  (let* ((file-content (get-string-from-file file))
         (year (regex-matches "year ?=.*" file-content)))
    (seq-map 'lautex--bib-info year)))


;;;###autoload
(defun lautex--form-candidates (file)
  (let ((citations (lautex--bib-citations file))
        (titles (lautex--bib-titles file))
        (years (lautex--bib-year file))
        (authors (lautex--bib-authors file)))
    (mapcar*
         (lambda (yea tit aut cit) (cons (format "(%s) %s\n%s" yea tit aut) cit))
        years titles authors citations)))


;;;###autoload
(defun lautex--citation-candidates ()
  "get all citation candidates of all bib files"
  (fp/pipe (lautex--get-bib-files)
     ((seq-map 'lautex--form-candidates)
      (apply 'append)
      (fp/alist-sort-by-car))))

;;;###autoload
(defun lautex--helm-citation-source ()
  (helm-build-sync-source "Label Source"
    :volatile t
    :multiline t
    :candidates (lautex--citation-candidates)
    :action 'lautex--insert-citation))

;;;###autoload
(defun LauTeX-insert-citation ()
  (interactive)
  (helm :prompt "Choose a source to cite: "
        :sources (lautex--helm-citation-source)
        :buffer "*helm buffer source*"))


(provide 'lautex)

;;
;; warnings
;;

;; In latex-define-preview-settings:
;; lautex.el:29:16:Warning: reference to free variable ‘org-format-latex-options’

;; In lautex--get-org-file-name:
;; lautex.el:70:12:Warning: function ‘reduce’ from cl package called at runtime

;; In lautex--get-text:
;; lautex.el:73:54:Warning: function ‘reduce’ from cl package called at runtime
;; lautex.el:107:7:Warning: assignment to free variable
;;     ‘lautex--helm-env-sources’
;; lautex.el:112:7:Warning: assignment to free variable
;;     ‘lautex--helm-env-sources-fallback’

;; In LauTeX-org-env:
;; lautex.el:126:10:Warning: ‘previous-line’ is for interactive use only; use
;;     ‘forward-line’ with negative argument instead.

;; In lautex--get-label:
;; lautex.el:163:27:Warning: function ‘reduce’ from cl package called at runtime
;; lautex.el:178:7:Warning: assignment to free variable ‘mock-org’

;; In lautex--build-reference-candidate:
;; lautex.el:198:44:Warning: function ‘reduce’ from cl package called at runtime

;; In lautex--get-citation:
;; lautex.el:246:30:Warning: function ‘reduce’ from cl package called at runtime

;; In lautex--bib-citations:
;; lautex.el:252:31:Warning: function ‘reduce’ from cl package called at runtime

;; In lautex--bib-info:
;; lautex.el:259:26:Warning: function ‘reduce’ from cl package called at runtime

;; In lautex--form-candidates:
;; lautex.el:293:77:Warning: function ‘mapcar*’ from cl package called at runtime

;; In lautex--citation-candidates:
;; lautex.el:302:8:Warning: function ‘reduce’ from cl package called at runtime

;; In end of data:
;; lautex.el:322:1:Warning: the following functions are not known to be defined: org-clear-latex-preview,
;;     org-latex-preview, org-beamer-export-to-pdf, org-latex-export-to-pdf,
;;     org-edit-special, org-edit-src-exit, org-toggle-latex-fragment,
;;     org-edit-src-save


