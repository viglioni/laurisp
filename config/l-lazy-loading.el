;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; This file contains calls to functions that are not
;; yet loaded by default (see laurisp.el)
;; all these functions will be overwritten by the loaded ones
;;


;;
;; Load essential libs for coding
;;
;;;###autoload
(defun prepare-ide ()
  (interactive)
  (message "loading flycheck...")
  (global-flycheck-mode)
  (message "loading company...")
  (global-company-mode)
  (message "loading yasnippets...")
  (add-to-list 'yas-snippet-dirs "~/laurisp/snippets")
  (yas-global-mode 1)
  (message "running exec path from shell...")
  (exec-path-from-shell-initialize)
  (load-lib 'launguage-server-protocol)
  (load-lib 'sqlau)
  (message "emacs IDE is ready"))

(spacemacs/set-leader-keys
  "@" 'prepare-ide)



;;
;; laurg
;;

;;;###autoload
(bind-lazy-function 'org-jira-copy-current-issue-url 'laurg-jira-copy-current-issue-url 'laurg)

;;
;; ramda docs
;;

;;;###autoload
(defun open-ramda-docs ()
  (interactive)
  (call-lazy-function 'open-ramda-docs 'ramda-docs))


;;
;; spotilau
;;

(defun spotify-status ()
  (interactive)
  (call-lazy-function 'spotify-status 'spotilau))

(defun spotify-helper ()
  (interactive)
  (call-lazy-function 'spotify-helper 'spotilau))

