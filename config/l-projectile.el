;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; projectile related functions
;;



(eval-after-load "projectile"
  (lambda ()
    (progn

      (setq projectile-globally-ignored-directories
            '(".git" ".svn" "out" "repl" "target" "venv" ".pub-cache" "node_modules" "ios" "android" "dist" ".next"))

      (setq projectile-globally-ignored-files
            '( ".DS_Store" "*.gz" "*.pyc" "*.jar" "*.tar.gz" "*.tgz" "*.zip" "*.png" ".packages" "*-lock.json" "*.chunk.*" ".lein-repl-history"))

      (setq projectile-project-search-path
            '("~/../Loft/" "~/../Personal/"))

      )))

