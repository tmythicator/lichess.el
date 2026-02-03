;;; lichess-broadcast-list.el --- List Lichess Broadcasts -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026  Alexandr Timchenko
;; URL: https://github.com/tmythicator/Lichess.el
;; Version: 0.8
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
(require 'lichess-api)
(require 'lichess-broadcast-view)
(require 'subr-x)

(defvar lichess-broadcast-list--buf "*Lichess Broadcasts*")

(defvar lichess-broadcast-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'lichess-broadcast-list)
    (define-key
     map (kbd "RET") #'lichess-broadcast-list-select-tournament)
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
  (let ((buf (get-buffer-create lichess-broadcast-list--buf)))
    (with-current-buffer buf
      (lichess-broadcast-list-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Fetching broadcasts...\n")))
    (pop-to-buffer buf)
    (lichess-api-get-broadcasts
     #'lichess-broadcast-list--render-list 20)))

(defun lichess-broadcast-list--render-list (res)
  "Render the broadcast list from RES."
  (let ((status (car res))
        (data (cdr res)))
    (when-let* ((buf (get-buffer lichess-broadcast-list--buf))
                (_ (buffer-live-p buf)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (if (/= status 200)
              (insert (format "Error fetching broadcasts: %s" status))
            (pcase-let ((`(,active ,upcoming)
                         (list
                          (lichess-util--aget data 'active)
                          (lichess-util--aget data 'upcoming))))

              (if active
                  (progn
                    (insert
                     (propertize "Active Broadcasts\n" 'face 'bold))
                    (insert (make-string 50 ?-) "\n")
                    (seq-do
                     #'lichess-broadcast-list--insert-item active))
                (insert "No active broadcasts.\n"))

              (insert "\n")
              (when upcoming
                (insert (make-string 50 ?-) "\n")
                (seq-do
                 #'lichess-broadcast-list--insert-item upcoming)))
            (goto-char (point-min))))))))

(defun lichess-broadcast-list--insert-item (item)
  "Insert a broadcast ITEM line."
  (let* ((tour (lichess-util--aget item 'tour))
         (round (lichess-util--aget item 'round)))
    (pcase-let* ((name
                  (or (lichess-util--aget tour 'name)
                      "Unknown Tournament"))
                 (round-name
                  (or (lichess-util--aget round 'name) "Round ?"))
                 (tour-id (lichess-util--aget tour 'id))
                 (round-id (lichess-util--aget round 'id))
                 (url (lichess-util--aget round 'url))
                 (line (format "%-40s | %s" name round-name)))
      (insert-button line
                     'action
                     #'lichess-broadcast-list-select-tournament
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
      (insert "\n"))))

(defun lichess-broadcast-list-select-tournament (&optional btn)
  "Select the broadcast at point or BTN."
  (interactive)
  (if-let* ((button (or btn (button-at (point))))
            (url (button-get button 'lichess-url))
            (round-id (button-get button 'lichess-round-id)))
    (lichess-broadcast-view-watch-round round-id url)
    (message "No broadcast found at point.")))

(provide 'lichess-broadcast-list)
;;; lichess-broadcast-list.el ends here
