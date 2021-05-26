;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; org related functions
;;

;;
;; Insert custom headers
;;



;;;###autoload
(defun require-headers-skeletons ()
  (require 'org-headers-skeletons))

;;;###autoload
(defun insert-custom-header (header-func)
  (save-excursion
    (beginning-of-buffer)
    (funcall (eval header-func))))

(setq helm-org-insert-custom-headers-sources
      (helm-build-sync-source "Avaliable headers:"
        :candidates '(("haskell notebook" . 'org-haskell-notebook-header)
                      ("beamer presentation" . 'org-beamer-presentations-header)
                      ("latex articles" . 'org-latex-article-header))
        :action 'insert-custom-header))

;;;###autoload
(defun org-insert-custom-headers ()
  (interactive)
  (throw-unless (derived-mode-p 'org-mode) "Not in org-mode!")
  (require-headers-skeletons)
  (helm :prompt "Choose a header: "
        :sources helm-org-insert-custom-headers-sources))


;;
;;  Insert source on org-mode
;;

;;;###autoload
(defun --org-insert-src (src-name &optional post-func)
  (let ((post (if post-func (concat ":post " post-func "(*this*)") "")))
    (insert (concat "#+begin_src " src-name " :exports both " post "\n\n" "#+end_src"))))

;;;###autoload
(defun org-insert-src (src-name)
  (cond
   ((string= "haskell" src-name) (--org-insert-src src-name "org-clear-haskell-output") )
   ((string= "clojure" src-name) (--org-insert-src src-name "org-clear-clojure-output") )
   (t (--org-insert-src src-name))))

(setq helm-org-sources
      (helm-build-sync-source "Language name"
        :candidates '(lambda () (mapcar 'car org-babel-load-languages))
        :action 'org-insert-src))

(setq helm-org-sources-fallback
      (helm-build-dummy-source "Language name"
        :action 'org-insert-src))

;;;###autoload
(defun insert-org-source ()
  (interactive)
  (if (eq 'org-mode major-mode)
      (progn
        (helm
         :history t
         :volatile nil
         :sources '(helm-org-sources helm-org-sources-fallback))
        (org-edit-special)
        (previous-line)
        (spacemacs/indent-region-or-buffer))))

;;
;; org jira 
;;

;;;###autoload
(defun org-jira-copy-current-issue-url ()
  (interactive)
  (let* ((issue-key (org-jira-get-from-org 'issue 'key))
         (issue-url (concat jiralib-url "/browse/" issue-key)))
    (kill-new issue-url)
    (message (concat "copied " issue-url))
    issue-url))

;;
;; Define org command by cursor position
;;

;;;###autoload
(defmacro define-org-cmd (&rest plist)
  "Receives a plist (:situation 'command)  as args to define which
   command should be called on each situation. 
   Obs.: the command will ONLY be called on the specific situation.
   *~*~*
   For now the supported keys are
   :heading -> runs when cursor is over a heading
   :table -> runs when cursor is over a table
   *~*~*
   example: (define-org-cmd :heading 'my-fn :table 'my-fn2)"
  (throw-if (oddp (length plist)) "arg list must have an even number of args")
  `(lambda ()
     (interactive)
     (cond
      ((org-at-heading-p) (funcall ,(plist-get plist :heading)))
      ((org-at-table-p)   (funcall ,(plist-get plist :table))))))

