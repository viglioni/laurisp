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
            '(
              ".cask"
              ".git"
              ".log"
              ".next"
              ".nyc_output"
              ".pub-cache"
              ".svn"
              ".vscode"
              "android"
              "dist"
              "dist-*"
              "ios"
              "node_modules"
              "out"
              "repl"
              "target"
              "venv"
              ))

      (setq projectile-globally-ignored-files
            '(
              "*-lock.json"
              "*.chunk.*"
              "*.gz"
              "*.jar"
              "*.log"
              "*.png"
              "*.pyc"
              "*.tar.gz"
              "*.tgz"
              "*.zip"
              ".DS_Store"
              ".lein-repl-history"
              ".packages"
              ))

      (setq projectile-project-search-path
            '("~/Loft/" "~/Personal/"))

      )))

