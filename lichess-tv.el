;;; lichess-tv.el --- Lichess TV integration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026  Alexandr Timchenko
;; URL: https://github.com/tmythicator/Lichess.el
;; Version: 0.7
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; See LICENSE for details.
;;
;;; Commentary:
;;
;; Live Lichess TV client:
;; - `M-x lichess-tv` to list all TV channels
;; - `M-x lichess-tv-debug` to inspect raw /api/tv/channels JSON
;; - `g` refreshes the list
;; - `RET` (planned) will open a game buffer
;;
;;; Code:

(require 'lichess-core)
(require 'lichess-util)
(require 'lichess-api)

(defcustom lichess-tv-fetch-delay 0.12
  "Delay between successive /api/game requests (seconds) to avoid HTTP 429."
  :type 'number
  :group 'lichess)

(defvar lichess-tv--buf "*Lichess TV*")
(defvar lichess-tv--next-at 0.0)

(defvar lichess-tv-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'lichess-tv)
    (define-key map (kbd "RET") #'lichess-tv-watch-game-at-point)
    (define-key map (kbd "<return>") #'lichess-tv-watch-game-at-point)
    (define-key map (kbd "C-m") #'lichess-tv-watch-game-at-point)
    map)
  "Keymap for `lichess-tv-mode'.")

(define-derived-mode
 lichess-tv-mode
 lichess-core-mode
 "Lichess-TV"
 "Major mode for the Lichess TV channel list."
 ;; All buffer setup is now centralized here.
 (setq truncate-lines t))

;;;###autoload
(defun lichess-tv ()
  "Show current Lichess TV channels; lines are clickable (RET)."
  (interactive)
  (let ((buf (get-buffer-create lichess-tv--buf)))
    (with-current-buffer buf
      (lichess-tv-mode))
    (lichess-core-with-buf
     buf (erase-buffer) (insert "Fetching Lichess TV channels…\n"))
    (pop-to-buffer buf))
  (lichess-api-get-tv-channels #'lichess-tv--handle-channels))

(defun lichess-tv-watch-game-at-point ()
  "Watch the Lichess game on the current line in a new buffer."
  (interactive)
  (let ((id (get-text-property (point) 'lichess-game-id)))
    (if id
        (lichess-game-watch id)
      (message "No game ID found on this line."))))

(defun lichess-tv--update-line (pos text &optional id)
  "Replace line at POS with TEXT.  If POS is stale, find the line by ID."
  (let ((buf (get-buffer lichess-tv--buf)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (save-excursion
            ;; 1) try marker position
            (let* ((p
                    (and (markerp pos)
                         (marker-buffer pos)
                         (marker-position pos)))
                   bol
                   cur-id)
              (when p
                (goto-char p)
                (setq
                 bol (line-beginning-position)
                 cur-id (get-text-property bol 'lichess-game-id)))
              ;; 2) fallback: locate by ID across the buffer
              (when (and id (not (equal id cur-id)))
                (let ((hit
                       (text-property-any
                        (point-min) (point-max) 'lichess-game-id id)))
                  (when hit
                    (goto-char hit)
                    (setq
                     bol (line-beginning-position)
                     cur-id id))))
              ;; 3) update if we found a line
              (when bol
                (let ((eol
                       (min (point-max) (1+ (line-end-position)))))
                  (delete-region bol eol)
                  (goto-char bol)
                  (lichess-util--insert-propertized-line
                   text id))))))))))


(defun lichess-tv--fetch-game (id chan-name marker)
  "Throttled fetch of /api/game/{ID} and update the line at MARKER.
CHAN-NAME is the channel label."
  (let* ((now (float-time))
         (at (max now lichess-tv--next-at)))
    (setq lichess-tv--next-at (+ at lichess-tv-fetch-delay))
    (run-at-time
     (- at now) nil
     (lambda ()
       (lichess-api-get-game
        id
        (lambda (res)
          (let ((status (car res))
                (data (cdr res)))
            (if (= status 200)
                (let ((vs (lichess-util--game->vs data)))
                  (lichess-tv--update-line marker
                                           (format
                                            "%-12s  %-64s  id:%s"
                                            chan-name vs id)
                                           id))
              (lichess-tv--update-line marker
                                       (format
                                        "%-12s  id:%s (HTTP %s)"
                                        chan-name id status)
                                       id)))))))))

(defun lichess-tv--insert-channel (pair)
  "Insert a placeholder for PAIR = (CHAN . GAME), then fetch full game and update."
  (pcase-let* ((`(,chan . ,g) pair)
               (chan-name (symbol-name chan))
               (id
                (or (lichess-util--aget g 'gameId)
                    (lichess-util--aget g 'id))))
    (lichess-core-with-buf
     (get-buffer lichess-tv--buf) (goto-char (point-max))
     (let* ((text (format "%-12s  %s  id:%s" chan-name "loading…" id))
            (marker (lichess-util--insert-propertized-line text id)))
       (when id
         (lichess-tv--fetch-game id chan-name marker))))))

(defun lichess-tv--handle-channels (res)
  "Process /api/tv/channels RES."
  (if (/= (car res) 200)
      (lichess-core-with-buf
       (get-buffer lichess-tv--buf)
       (erase-buffer)
       (insert (format "HTTP %s from /api/tv/channels\n" (car res))))
    (mapc #'lichess-tv--insert-channel (cdr res))))

(provide 'lichess-tv)
;;; lichess-tv.el ends here
