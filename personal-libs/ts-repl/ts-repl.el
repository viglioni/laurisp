;;; ts-repl.el --- Ts Repl  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Laura Viglioni

;; Author: Laura Viglioni <viglionilaura@gmail.com>
;; Maintainer: Laura Viglioni <viglionilaura@gmail.com>
;; Created: 04 Jul 2021
;; Keywords: keywods
;; URL: https://github.com/Viglioni/laurisp/tree/main/personal-libs/ts-repl
;; Version:  0.0.1
;; Package-Requires: ((emacs "24.1"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;

;;; Commentary:
;;  comentary

;;; Code:

(load-lib 'laurisp-core)
(load-lib 'functional)

(defvar ts-repl--buffer-name "*TS-result*")

;;;###autoload
(defun ts-repl--is-ts? (filename)
  "Checks if file is a typescript file"
  (bool (string-match "\\.tsx?$" filename)))

;;;###autoload
(define-derived-mode ts-repl-mode typescript-mode "TS Repl"
  "A major mode for visualizing the output of a TS file")


;;;###autoload
(defun ts-repl--console (beginning end)
  (concat "\nconsole.log("
          (replace-regexp-in-string
           ";$" ""
           (buffer-substring beginning end))
          ")\n"))


;;;###autoload
(defun ts-repl--pulse (&optional sexp-beg sexp-end)
  (pulse-momentary-highlight-region
   (or sexp-beg (and (use-region-p) (region-beginning)) (point-min))
   (or sexp-end (and (use-region-p) (region-end)) (point-max))))


;;;###autoload
(defun ts-repl--content (&optional beginning-reg end-reg)
  (let* ((buffer-content
          (buffer-substring-no-properties (point-min) (point-max)))
         (selected-region? (bool (and beginning-reg end-reg)))
         (region? (bool (or selected-region? (use-region-p))))
         (beginning (if selected-region? beginning-reg (region-beginning)))
         (end (if selected-region? end-reg (region-end)))
         (console-content
          (if region? (ts-repl--console beginning end) "")))

    (concat buffer-content console-content)))

;;;###autoload
(defun ts-repl--run-ts (tmp-file)
  (async-shell-command
     (concat "echo \"-*-TS-Repl-start-*-\n\n\""
             " && npx ts-node -T " tmp-file
             " && echo \"\n\n-*-TS-Repl-end-*-\"")
     ts-repl--buffer-name))

;;;###autoload
(defun ts-repl--run-after-ts (tmp-file)
  "Commands that will run after the TS execution"
  (let* ((result-buff (get-buffer ts-repl--buffer-name))
         (proc (get-buffer-process result-buff)))
    (when (process-live-p proc)
      (set-process-sentinel
       proc
       #'(lambda (process signal)
           (with-current-buffer result-buff (funcall 'ts-repl-mode))
           (shell-command (concat "rm " tmp-file))
           (shell-command-sentinel process signal))))))

;;;###autoload
(defun ts-repl-send-last-sexp ()
  (interactive)
  (let ((beginning (save-excursion
                     (backward-sexp)
                     (move-beginning-of-line nil)
                     (point)))
        (end (point)))
    (ts-repl-exec-ts-buffer beginning end)))

;;;###autoload
(defun ts-repl-exec-ts-buffer (&optional sexp-beg sexp-end)
  "Executes buffer without type verification for efficiency reasons"
  (interactive)
  (let* ((result-buff (get-buffer-create ts-repl--buffer-name))
         (ts-content (ts-repl--content sexp-beg sexp-end))
         (tmp-file (concat (make-temp-name "ts-repl") ".ts")))
    (throw-unless (ts-repl--is-ts? (buffer-file-name))
                  "this is not a typescript file!")
    (write-region ts-content nil tmp-file)
    (ts-repl--pulse sexp-beg sexp-end)
    (ts-repl--run-ts tmp-file)
    (ts-repl--run-after-ts tmp-file)))


(provide 'ts-repl)
;;; ts-repl.el ends here






