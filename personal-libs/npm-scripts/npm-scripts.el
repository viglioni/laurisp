;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

(message "loading npm-scripts...")

(require 'helm)
(require 'seq)
(load-lib 'functional)
(load-lib 'json-utils)

;;
;; Get scripts from package.json
;;

;;;###autoload
(defun NS--get-scripts (package-json)
  "@param package.json filepath
   @returns a list of all scripts in package.json"
  (fp/pipe package-json
    ((json-read-file)
     (alist-get 'scripts)
     (fp/alist-sort-by-car)
     (mapcar 'car))))

;;;###autoload
(defun NS--build-scripts (scripts-list)
  "@param a list of strings
   @returns an alist of (scriptname . \"npm run script\")"
  (mapcar (lambda (script) (cons (format "%s" script)
                            (format "npm run %s" script)))
          scripts-list))

;;
;; Build shell buffer to run the script
;;

;;;###autoload
(defun NS--open-buffer (buff)
  (display-buffer-in-side-window buff '((side . bottom))))

;;;###autoload
(defun NS--run-script (script-cmd)
  "@param (string) a npm command e.g. \"npm run dev\"
   Runs this script in a dedicated async shell buffer"
  (let* ((cmd-name (replace-regexp-in-string " " "-"  script-cmd))
         (buff-name (concat "*:" (projectile-project-name) "::" cmd-name ":*"))
         (err-buff-name (concat "*:ERROR::" (projectile-project-name) "::" cmd-name ":*"))
         (buff (get-buffer-create buff-name)))
    (NS--open-buffer buff)
    (async-shell-command script-cmd buff-name err-buff-name)
    (set-window-dedicated-p (get-buffer-window buff) t)))

;;;###autoload
(defun NS--is-npm-buff? (buff-or-buff-name)
  "Checks if a buffer name matches the regex of this lib created buffers
   @param (string | buffer)
   @returns bool"
  (let ((buff-name (if (stringp buff-or-buff-name) buff-or-buff-name
                     (buffer-name buff-or-buff-name))))
    (bool (regex-matches (rx (and "*:" (+ (or alphanumeric "-")) "::" (+ (or alphanumeric "-")) ":*")) buff-name))))

;;;###autoload
(defun NS--get-buffer ()
  "Gets language script buffer, if any, throws otherwise
   () -> buffer | error"
  (let* ((bottom-window (purpose-get-bottom-window))
         (buff-name (and bottom-window (buffer-name (window-buffer bottom-window))))
         (is-npm-buff? buff-name))
    (throw-unless is-npm-buff? "no language script buffer is found")
    (get-buffer buff-name)))


;;;###autoload
(defun NS--active-buffers-alist ()
  "@returns an alist (buffer-name . buffer)"
  (fp/pipe (buffer-list)
    ((seq-filter 'NS--is-npm-buff? )
     (mapcar (lambda (buff) (cons (buffer-name buff) buff))))))

;;;###autoload
(defun npm-scripts:hide-buffer ()
  "Hides a buffer with a npm command running.
    It will hide only if it is on the bottom window and matches the regex"
  (interactive)
  (NS--get-buffer)
  (purpose-delete-window-at-bottom))

;;;###autoload
(defun language-scripts:go-to-buffer ()
  "Focus on buffer or throws if no buffer is found"
  (interactive)
  (fp/pipe (NS--get-buffer)
    ((get-buffer-window)
     (select-window)))
  (goto-char (point-max)))

;;
;; Helm functions
;;

;; npm-scripts:choose-and-run

;;;###autoload
(defun NS--helm-candidates ()
  "Gets the script from package.json and returns an alist (name . command)"
  (fp/pipe (json-utils-get-package-json)
    ((NS--get-scripts)
     (NS--build-scripts))))

;;;###autoload
(defun NS--helm-scripts-source ()
  (helm-build-sync-source "Avaliable scripts on your package.json: "
    :volatile t
    :multiline nil
    :candidates (NS--helm-candidates)
    :action 'NS--run-script))

;;;###autoload
(defun npm-scripts:choose-and-run ()
  "Lists all avaliable scripts on package.json and runs the selected one"
  (interactive)
  (helm :prompt "Choose a script to run: "
        :sources (NS--helm-scripts-source)
        :buffer "*helm avaliable npm scripts*"))

;; npm-scripts:open-active-buffer

(defun NS--helm-buffer-source ()
  (helm-build-sync-source "Active npm buffers: "
    :volatile t
    :multiline nil
    :candidates (NS--active-buffers-alist)
    :action 'NS--open-buffer))

(defun npm-scripts:open-active-buffer ()
  "Lists all active npm buffers and opens the selected one"
  (interactive)
  (helm :promp "Choose a buffer to open: "
        :buffer "*helm active npm buffers*"
        :sources (NS--helm-buffer-source)))

(provide 'npm-scripts)

;;
;; compiler warnings
;;

;; In NS--get-scripts:
;; npm-scripts.el:19:25:Warning: function ‘reduce’ from cl package called at
;; runtime

;; In NS--run-script:
;; npm-scripts.el:45:24:Warning: function ‘reduce’ from cl package called at
;; runtime

;; In NS--active-buffers-alist:
;; npm-scripts.el:81:15:Warning: function ‘reduce’ from cl package called at
;; runtime

;; In NS--helm-candidates:
;; npm-scripts.el:91:8:Warning: function ‘reduce’ from cl package called at
;; runtime

