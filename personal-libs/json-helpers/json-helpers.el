;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

(message "loading json-helpers...")

(require 'json)
(require 'projectile)

;;;###autoload
(defun JH--package-json ()
  "Gets package.json on the root of the project"
  (let ((json-file (join-path (or (projectile-project-root) ".") "package.json")))
    (throw-unless (file-exists-p json-file) "package.json not found!")
    json-file))


(provide 'json-helpers)
