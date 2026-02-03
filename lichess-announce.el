;;; lichess-announce.el --- Accessibility announcements for Lichess -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026  Alexandr Timchenko
;; URL: https://github.com/tmythicator/Lichess.el
;; Version: 0.8
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; Accessibility features for Lichess.el, including move announcements
;; for screen readers.
;;
;;; Code:

(require 'lichess-core)

(defgroup lichess-announce nil
  "Accessibility features for Lichess."
  :group 'lichess)

(defcustom lichess-announce-events nil
  "When non-nil, announce game events in the echo area.
This is useful for screen reader users to track live games or
history navigation."
  :type 'boolean
  :group 'lichess-announce)

(defcustom lichess-announce-use-buffer nil
  "When non-nil, log all announcements to a persistent buffer.
The buffer is named *Lichess Announcer*."
  :type 'boolean
  :group 'lichess-announce)

(defconst lichess-announce-buffer-name "*Lichess Announcer*"
  "Name of the buffer for accessibility logs.")

(defun lichess-announce--log (msg)
  "Append MSG to the *Lichess Announcer* buffer if enabled."
  (when lichess-announce-use-buffer
    (let ((inhibit-read-only t)
          (buf (get-buffer-create lichess-announce-buffer-name))
          (time-str (format-time-string "[%H:%M:%S] ")))
      (with-current-buffer buf
        (unless (derived-mode-p 'special-mode)
          (special-mode))
        (save-excursion
          (goto-char (point-max))
          (insert time-str msg "\n"))))))

(defun lichess-announce-game-move (moves-str idx stm)
  "Announce the move at IDX from MOVES-STR.
STM is the side-to-move symbol (\`w' or \`b').
This is a high-level helper for `lichess-game-render'."
  (when lichess-announce-events
    (let ((side
           (if (eq stm 'w)
               "White"
             "Black")))
      (if (= idx 0)
          (lichess-announce-event
           (format "Starting position. %s to move." side))
        (let* ((moves (split-string (or moves-str "") " " t))
               (move (nth (1- idx) moves))
               (move-num (1+ (/ (1- idx) 2)))
               (is-white (eq (% (1- idx) 2) 0))
               (who-played
                (if is-white
                    "White"
                  "Black")))
          (lichess-announce-move
           move-num is-white move side who-played))))))

(defun lichess-announce-move
    (move-num is-white move side-to-move who-played)
  "Announce a move in the echo area if \`lichess-announce-events' is enabled.
MOVE-NUM: The move number.
IS-WHITE: Non-nil if it was a white move.
MOVE: The UCI move string.
SIDE-TO-MOVE: String indicating whose turn it is now.
WHO-PLAYED: String indicating who made the move."
  (when lichess-announce-events
    (let ((msg
           (format "%s played %s. %s to move (Move %d%s)."
                   who-played (or move "?") side-to-move move-num
                   (if is-white
                       ""
                     "..."))))
      (message "%s" msg)
      (lichess-announce--log msg))))

(defun lichess-announce-event (message)
  "Announce a generic accessibility EVENT message.
MESSAGE: The string to announce."
  (when lichess-announce-events
    (let ((msg (format "Lichess: %s" message)))
      (message "%s" msg)
      (lichess-announce--log msg))))

(provide 'lichess-announce)
;;; lichess-announce.el ends here
