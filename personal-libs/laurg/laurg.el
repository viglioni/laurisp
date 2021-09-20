;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; org related functions
;;

(require 'helm)
(message "loading laurg...")

;;
;; Insert custom headers
;;


;;;###autoload
(defun laurg--insert-custom-header (header-func)
  (save-excursion
    (beginning-of-buffer)
    (funcall (eval header-func))))

(setq laurg--helm-insert-custom-headers-srcs
      (helm-build-sync-source "Avaliable headers:"
        :candidates '(("haskell notebook" . 'org-haskell-notebook-header)
                      ("beamer presentation" . 'org-beamer-presentations-header)
                      ("latex articles" . 'org-latex-article-header))
        :action 'laurg--insert-custom-header))

;;;###autoload
(defun laurg-insert-custom-headers ()
  (interactive)
  (throw-unless (derived-mode-p 'org-mode) "Not in org-mode!")
  (load-lib 'org-headers-skeletons)
  (helm :prompt "Choose a header: "
        :sources laurg--helm-insert-custom-headers-srcs))


;;
;;  Insert source on org-mode
;;

;;;###autoload
(defun laurg--insert-src-with-post (src-name &optional post-func)
  (let ((post (if post-func (concat ":post " post-func "(*this*)") "")))
    (insert (concat "#+begin_src " src-name " :exports both :results output" post
                    "\n\n"
                    "#+end_src"))))

;;;###autoload
(defun laurg--insert-src (src-name)
  (cond
   ((string= "haskell" src-name) (laurg--insert-src-with-post src-name "org-babel-haskell-formatter"))
   ((string= "clojure" src-name) (laurg--insert-src-with-post src-name "org-babel-clojure-formatter"))
   (t (laurg--insert-src-with-post src-name))))

(setq laurg--helm-lang-sources
      (helm-build-sync-source "Language name"
        :candidates '(lambda () (mapcar 'car org-babel-load-languages))
        :action 'laurg--insert-src))

(setq laurg--helm-lang-sources-fallback
      (helm-build-dummy-source "Language name"
        :action 'laurg--insert-src))

;;;###autoload
(defun laurg-insert-org-source ()
  (interactive)
  (if (eq 'org-mode major-mode)
      (progn
        (helm
         :history t
         :volatile nil
         :sources '(laurg--helm-lang-sources laurg--helm-lang-sources-fallback))
        (org-edit-special)
        (previous-line)
        (spacemacs/indent-region-or-buffer))))

;;
;; org jira 
;;

;;;###autoload
(defun laurg-jira-copy-current-issue-url ()
  (interactive)
  (let* ((issue-key (org-jira-get-from-org 'issue 'key))
         (issue-url (concat jiralib-url "/browse/" issue-key)))
    (kill-new issue-url)
    (message (concat "copied " issue-url))
    issue-url))


(provide 'laurg)

;;
;; warnings
;;

;; In insert-custom-header:
;; laurg.el:23:30:Warning: ‘beginning-of-buffer’ is for interactive use only; use
;; ‘(goto-char (point-min))’ instead.
;; laurg.el:28:7:Warning: assignment to free variable
;; ‘laurg--helm-insert-custom-headers-srcs’

;; In org-insert-custom-headers:
;; laurg.el:41:18:Warning: reference to free variable
;; ‘laurg--helm-insert-custom-headers-srcs’
;; laurg.el:60:7:Warning: assignment to free variable ‘laurg--helm-lang-sources’
;; laurg.el:65:7:Warning: assignment to free variable ‘laurg--helm-lang-sources-fallback’

;; In insert-org-source:
;; laurg.el:78:10:Warning: ‘previous-line’ is for interactive use only; use
;; ‘forward-line’ with negative argument instead.

;; In define-org-cmd:
;; laurg.el:110:27:Warning: function ‘oddp’ from cl package called at runtime

;; In end of data:
;; laurg.el:118:1:Warning: the following functions are not known to be defined: org-edit-special,
;; org-jira-get-from-org
