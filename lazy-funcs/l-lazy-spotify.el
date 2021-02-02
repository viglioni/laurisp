;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; lazy-spotify related functions
;;

;;;###autoload
(defun spotify-share-song ()
  (let* ((cmd-ret (shell-command-to-string "spotify share url"))
         (url (replace-regexp-in-string "Spotify URL: " "" cmd-ret)))
    (kill-new url)
    (print "Url is copied into your clipboard")))

;;;###autoload
(defun kill-spotify-buffer (buffer-name)
  (if (get-buffer buffer-name) (kill-buffer buffer-name)))

;;;###autoload
(defun get-spotify-status ()
  (shell-command-to-string "spotify status"))

;;;###autoload
(defun get-spotify-vol ()
  (shell-command-to-string "spotify vol"))

;;;###autoload
(defun spotify-volume-down ()
  (shell-command-to-string "spotify vol down"))

;;;###autoload
(defun spotify-volume-up ()
  (shell-command-to-string "spotify vol up"))

;;;###autoload
(defun insert-spotify-buffer-content (buffer-name status vol)
  (let ((inhibit-read-only t)
        (status-formatted (compose
                           '('(replace-regexp-in-string "Artist" "\nArtist")
                              '(replace-regexp-in-string "Position: .*\n" ""))
                           status)))
    (with-current-buffer
        buffer-name
      (erase-buffer)
      (insert (concat status-formatted "\n" vol)))))

;;;###autoload
(defun spotify-helper (x)
  (interactive "k(t) Play/Pause; (n) Next; (p) Previous; (/) Search; (s) Share; (u) Vol Up; (d) Vol Down; (any) Quit helper" )
  (if (cond
       ((equal x "u") (spotify-volume-up))
       ((equal x "d") (spotify-volume-down))
       ((equal x "t") (spotify-playpause))
       ((equal x "n") (spotify-next))
       ((equal x "p") (spotify-previous))
       ((equal x "/") (helm-spotify-plus))
       ((equal x "s") (spotify-share-song) nil))
      (if (get-buffer "spotify-status")
          (call-interactively 'spotify-status)
        (call-interactively 'spotify-helper))
    (kill-spotify-buffer "spotify-status")))

;;;###autoload
(defun show-spotify-status ()
  (let* ((buffer-name "spotify-status")
         (status (get-spotify-status))
         (vol (get-spotify-vol)))
    (if (not (get-buffer buffer-name)) (get-buffer-create buffer-name))
    (display-buffer-in-side-window (get-buffer buffer-name) display-buffer-alist)
    (select-window (get-buffer-window buffer-name))
    (insert-spotify-buffer-content buffer-name status vol)
    (toggle-read-only)))

;;;###autoload
(defun spotify-status ()
  (interactive)
  (show-spotify-status)
  (call-interactively 'spotify-helper))
