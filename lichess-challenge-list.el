;;; lichess-challenge-list.el --- UI for managing challenges -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026  Alexandr Timchenko
;; URL: https://github.com/tmythicator/Lichess.el
;; Version: 0.8
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; UI for listing and managing (accept/cancel) Lichess challenges.
;;

;;; Code:

(require 'cl-lib)
(require 'lichess-util)
(require 'lichess-api)
(require 'lichess-game)

(defvar lichess-challenge-list--challenges nil
  "Vector of current challenge objects.")

(defvar lichess-challenge-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'lichess-challenge-list-cancel)
    (define-key map (kbd "a") #'lichess-challenge-list-accept)
    (define-key map (kbd "g") #'lichess-challenge-list-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `lichess-challenge-list-mode'.")

(define-derived-mode
 lichess-challenge-list-mode
 special-mode
 "Lichess Challenges"
 "Major mode for listing Lichess challenges."
 (setq truncate-lines t))

;;;###autoload
(defun lichess-challenge-list ()
  "List outgoing and incoming challenges."
  (interactive)
  (let ((buf (get-buffer-create "*Lichess Challenges*")))
    (with-current-buffer buf
      (lichess-challenge-list-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Loading challenges...\n")))
    (pop-to-buffer buf)
    (lichess-challenge-list-refresh)))

(defun lichess-challenge-list-refresh ()
  "Refresh the challenge list from the API."
  (interactive)
  (let ((buf (get-buffer-create "*Lichess Challenges*")))
    (lichess-api-get-challenges
     (lambda (res)
       (let ((status (car res))
             (data (cdr res)))
         (if (= status 200)
             (let ((in (lichess-util--aget data 'in))
                   (out (lichess-util--aget data 'out)))
               ;; Flatten and convert to vector
               (setq lichess-challenge-list--challenges
                     (vconcat in out))
               (lichess-challenge-list--render buf))
           (with-current-buffer buf
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert
                (format "Error fetching challenges: %d"
                        status))))))))))

(defun lichess-challenge-list--render (buf)
  "Render `lichess-challenge-list--challenges' into BUF."
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert
       (format "Lichess Challenges (%s)\n\n"
               (format-time-string "%T")))

      (if (= (length lichess-challenge-list--challenges) 0)
          (insert "  (No active challenges)\n")
        (insert
         "  ID           Challenger      Opponent        Perf       Status\n")
        (insert
         "  --------------------------------------------------------------\n")
        (cl-loop
         for
         i
         from
         0
         below
         (length lichess-challenge-list--challenges)
         do
         (lichess-challenge-list--insert-item
          i (aref lichess-challenge-list--challenges i))))

      (insert "\n[a] Accept  [c] Cancel  [g] Refresh  [q] Quit"))))

(defun lichess-challenge-list--insert-item (index ch)
  "Insert a challenge item line for challenge CH at INDEX."
  (let* ((id (lichess-util--aget ch 'id))
         (challenger
          (lichess-util--aget
           (lichess-util--aget ch 'challenger) 'name))
         (dest
          (lichess-util--aget
           (lichess-util--aget ch 'destUser) 'name))
         (perf
          (lichess-util--aget (lichess-util--aget ch 'perf) 'name))
         (status (lichess-util--aget ch 'status))
         ;; Using simple columns for now
         (line
          (format "  %-12s %-15s %-15s %-10s %s\n"
                  id
                  (or challenger "?")
                  (or dest "?")
                  (or perf "?")
                  status)))
    (insert (propertize line 'lichess-challenge-index index))))

(defun lichess-challenge-list-cancel ()
  "Cancel the challenge at point."
  (interactive)
  (let ((idx (get-text-property (point) 'lichess-challenge-index)))
    (if idx
        (let* ((ch (aref lichess-challenge-list--challenges idx))
               (id (lichess-util--aget ch 'id)))
          (when (y-or-n-p (format "Cancel challenge %s? " id))
            (message "Cancelling challenge %s..." id)
            (lichess-api-cancel-challenge
             id
             (lambda (res)
               (let ((status (car res)))
                 (if (= status 200)
                     (message "Challenge cancelled.")
                   (message "Error cancelling challenge: %d" status))
                 (lichess-challenge-list-refresh)))))) ;; Refresh
      (message "No challenge at point."))))

(defun lichess-challenge-list-accept ()
  "Accept the challenge at point."
  (interactive)
  (let ((idx (get-text-property (point) 'lichess-challenge-index)))
    (if idx
        (let* ((ch (aref lichess-challenge-list--challenges idx))
               (id (lichess-util--aget ch 'id)))
          (when (y-or-n-p (format "Accept challenge %s? " id))
            (message "Accepting challenge %s..." id)
            (lichess-api-accept-challenge
             id
             (lambda (res)
               (let ((status (car res)))
                 (if (= status 200)
                     (progn
                       (message
                        "Challenge accepted! Starting game %s..."
                        id)
                       (lichess-game-play id))
                   (message "Error accepting challenge: %d" status))
                 (lichess-challenge-list-refresh)))))) ;; Refresh
      (message "No challenge at point."))))

(provide 'lichess-challenge-list)
;;; lichess-challenge-list.el ends here
