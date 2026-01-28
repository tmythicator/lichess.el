;;; lichess-broadcast-list.el --- List Lichess Broadcasts -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026  Alexandr Timchenko
;; URL: https://github.com/tmythicator/Lichess.el
;; Version: 0.6
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; List active and upcoming Lichess Broadcasts.
;;
;;; Code:

(require 'lichess-core)
(require 'lichess-util)
(require 'lichess-http)
(require 'lichess-broadcast-view)

(defvar lichess-broadcasts--buf "*Lichess Broadcasts*")

(defvar lichess-broadcast-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'lichess-broadcast-list)
    (define-key map (kbd "RET") #'lichess-broadcast-select-tournament)
    map)
  "Keymap for `lichess-broadcast-list-mode'.")

(define-derived-mode
  lichess-broadcast-list-mode
  lichess-core-mode
  "Lichess-Broadcasts"
  "Mode for listing Lichess Broadcasts."
  (setq truncate-lines t))

;;;###autoload
(defun lichess-broadcast-list ()
  "List active and top Lichess broadcasts."
  (interactive)
  (let ((buf (get-buffer-create lichess-broadcasts--buf)))
    (with-current-buffer buf
      (lichess-broadcast-list-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Fetching broadcasts...\n")))
    (pop-to-buffer buf)
    (lichess-http-json
     "/api/broadcast/top?nb=20" #'lichess-broadcast--render-list)))

(defun lichess-broadcast--render-list (res)
  "Render the broadcast list from RES."
  (let ((status (car res))
        (data (cdr res))
        (buf (get-buffer lichess-broadcasts--buf)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (if (/= status 200)
              (insert (format "Error fetching broadcasts: %s" status))
            (let ((active (lichess-util--aget data 'active))
                  (upcoming (lichess-util--aget data 'upcoming)))

              (if active
                  (progn
                    (insert
                     (propertize "Active Broadcasts\n" 'face 'bold))
                    (insert (make-string 50 ?-) "\n")
                    (mapc #'lichess-broadcast--insert-item active))
                (insert "No active broadcasts.\n"))

              (insert "\n")
              (when upcoming
                (insert (make-string 50 ?-) "\n")
                (mapc
                 #'lichess-broadcast--insert-item upcoming)))
            (goto-char (point-min))))))))

(defun lichess-broadcast--insert-item (item)
  "Insert a broadcast ITEM line."
  (let* ((tour (lichess-util--aget item 'tour))
         (round (lichess-util--aget item 'round))
         (name (lichess-util--aget tour 'name))
         (round-name (lichess-util--aget round 'name))
         (tour-id (lichess-util--aget tour 'id))
         (round-id (lichess-util--aget round 'id))
         (url (lichess-util--aget round 'url))
         (line (format "%-40s | %s" name round-name)))
    (insert-button line
                   'action
                   #'lichess-broadcast-select-tournament
                   'lichess-tour-id
                   tour-id
                   'lichess-round-id
                   round-id
                   'lichess-url
                   url
                   'follow-link
                   t
                   'face
                   'default)
    (insert "\n")))

(defun lichess-broadcast-select-tournament (&optional btn)
  "Select the broadcast at point or BTN."
  (interactive)
  (let ((button (or btn (button-at (point)))))
    (if button
        (let ((url (button-get button 'lichess-url))
              (round-id (button-get button 'lichess-round-id)))
          (if (and url round-id)
              (lichess-broadcast-watch-round round-id url)
            (message "Button missing broadcast info.")))
      (message "No broadcast found at point."))))

(provide 'lichess-broadcast-list)
;;; lichess-broadcast-list.el ends here
