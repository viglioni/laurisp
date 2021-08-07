;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; files related functions
;;

(load-lib 'org-headers-skeletons)

;;
;; opening files
;;

;;;###autoload
(defun open-spacemacs-config ()
  "opens .spacemacs file"
  (interactive)
  (find-file "~/.spacemacs"))

;;;###autoload
(defun open-skhd-config ()
  "opens .skhdrc file"
  (interactive)
  (find-file "~/.skhdrc"))

;;;###autoload
(defun open-yabai-config ()
  "opens .yabairc file"
  (interactive)
  (find-file "~/.yabairc"))

;;;###autoload
(defun open-zsh-config ()
  "opens .zshrc file"
  (interactive)
  (find-file "~/.zshrc"))

;;;###autoload
(defun open-laurisp ()
  "opens laurisp.el file"
  (interactive)
  (find-file "~/laurisp/laurisp.el"))

;;
;; creating files
;;

;;;###autoload
(defun new-laurisp-file (name dir)
  "create laurisp file"
  (interactive "sInsert filename: \nDWhere? ")
  (let* ((filename (concat "l-" name ".el"))
         (file (touch filename dir)))
    (echo-into file (format ";;\n;; @author Laura Viglioni\n;; 2021\n;; GNU Public License 3.0\n;;\n\n;;\n;; %s related functions\n;;\n\n" name))
    (find-file file)))


(create-skeleton lib-readme
  "  To run tests and compiling this project uses [[https://github.com/doublep/eldev][eldev]]."
  ""
  "* Testing"
  "To run tests we use [[https://github.com/jorgenschaefer/emacs-buttercup/][buttercup]] and some macros defined in ~~/test/buttercup-helpers~, check out [[https://github.com/Viglioni/laurisp/tree/main/personal-libs/buttercup-helpers][buttercup-helpers repo]]."
  "*** To run tests"
  "    In the root of this project:"
  "    #+begin_src shell"
  "      $ eldev test"
  "    #+end_src"
  "* Compiling"
  "*** To compile"
  "    In the root of this project:"
  "    #+begin_src shell"
  "      $ eldev compile"
  "    #+end_src")


;;;###autoload
(defun new-laurisp-lib (file-name)
  (interactive "sInsert filename: ")
  (let* ((dir (join-path personal-lib-dir file-name))
         (test-dir (join-path dir "test")))
    (make-directory dir)
    (make-directory test-dir)
    (let ((file-path (touch (concat file-name ".el") dir))
          (readme (touch "README.org" dir))
          (eldev (touch "Eldev" dir))
          (test-file (touch (concat file-name "-test.el"))))
      (echo-into file-path "laurisp-package")
      (echo-into readme (concat "* " file-name))
      (echo-into eldev "(eldev-use-package-archive 'melpa)\n;(eldev-use-local-dependency \"\")")
      (echo-into test-file )
      (find-file file-path)
      (end-of-line)
      (yas-expand))))


