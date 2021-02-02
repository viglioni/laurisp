;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; js related functions
;;

;; (defun jsx (filename)
;;   "returns filename.jsx"
;;   (file-extension filename "jsx"))

;; (defun js (filename)
;;   "returns filename.js"
;;   (file-extension filename "js"))

;; (defun jsx-test (filename)
;;   "returns filename.spec.jsx"
;;   (file-extension filename "spec.jsx"))


;; (defun react-template (component-name)
;;   "Creates a react component string template"
;;   (format "import React from 'react'
;; import PropTypes from 'prop-types'
;; import helpers from '../helpers'
;; \n\n
;; const {connectWithRedux} = helpers
;; \n\n
;; const %s = ({}) => {
;;   return (<div/>)
;; };\n
;; %s.propTypes = {};

;; export default connectWithRedux({
;;   states: {},
;;   actions: {}
;; })(%s);" component-name component-name component-name))



;; (defun create-react-component (component-name dir)
;;   ""
;;   (interactive "sInsert component name: \nDWhere? " )
;;   (let* ((filename (jsx component-name))
;;          (file (touch filename dir)))
;;     (echo-into file (react-template component-name))))

 
;; (defun import-file-js (component file-name)
;;   ""
;;   (interactive "sWhat do you want to import? \nfFrom? ")
;;   (let* ((path (remove-suffix (relative-path (buffer-file-name) file-name)))
;;          (imp (format "import %s from '%s';\n" component path)))
;;     (insert-text-on-first-empty-line imp (point))))


;; (defun import-lib-js (component lib)
;;   "adds 'import component from lib' or adds a new component to an existing line 'import {component, ..} from lib'"
;;   (interactive "sInsert component name: \nsFrom? ")
;;   (let* ((full-imp (format "import %s from '%s';\n" component lib))
;;          (comp-imp (format "%s, " component))
;;          (current-point (point)))
;;     (progn
;;       (beginning-of-buffer)
;;       (if (re-search-forward (format "^import .* from .%s" lib) nil t)
;;           (progn
;;             (re-search-backward "{")
;;             (goto-char (inc (point)))
;;             (insert comp-imp)
;;             (goto-char (+ current-point (length comp-imp))))
;;         (insert-text-on-first-empty-line full-imp current-point)))))


;; ;;
;; ;; eslint configuration
;; ;;

;; (defun use-local-eslint ()
;;   (let* ((root (projectile-project-root))
;;          (eslint (and root (join-path root "node_modules/.bin/eslint"))))
;;     (when (and eslint (file-executable-p eslint))
;;       (setq-local flycheck-javascript-eslint-executable eslint))))

;; (add-hook 'flycheck-mode-hook #'use-local-eslint)

;; (defun run-lint-on-save ()
;;   (let* ((modes '(rjsx-mode js2-mode typescript-mode web-mode))
;;          (root (or (projectile-project-root) ""))
;;          (eslint (join-path root "node_modules/.bin/eslint"))
;;          (file (buffer-file-name))
;;          (linter (format "%s %s --fix" eslint file)))
;;     (when (and (contains modes major-mode) (file-executable-p eslint))
;;       (shell-command linter))))

;; ;(add-hook 'after-save-hook #'run-lint-on-save)


