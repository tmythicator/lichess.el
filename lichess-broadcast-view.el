;;; lichess-broadcast-view.el --- Watch Lichess Broadcast -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026  Alexandr Timchenko
;; URL: https://github.com/tmythicator/Lichess.el
;; Version: 0.6
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; Watch a specific Lichess Broadcast round in a grid view with auto-updates.
;;
;;; Code:

(require 'lichess-core)
(require 'lichess-util)
(require 'lichess-http)
(require 'lichess-game)
(require 'lichess-board-gui)

;;; Polling & Grid View

(defvar-local lichess-broadcast--timer nil)
(defvar-local lichess-broadcast--round-id nil)
(defvar-local lichess-broadcast--round-url nil)
(defvar-local lichess-broadcast--games nil)

(defvar lichess-broadcast-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `lichess-broadcast-mode'.")

(define-derived-mode
  lichess-broadcast-mode
  lichess-core-mode
  "Lichess-Broadcast"
  "Mode for watching a live broadcast round."
  (setq truncate-lines t)
  ;; Ensure timer is cancelled when buffer is killed
  (add-hook 'kill-buffer-hook #'lichess-broadcast--cleanup-timer
            nil
            t))

(defun lichess-broadcast--cleanup-timer ()
  "Stop the broadcast timer."
  (when lichess-broadcast--timer
    (cancel-timer lichess-broadcast--timer)
    (setq lichess-broadcast--timer nil)))

(defun lichess-broadcast-watch-round (round-id url)
  "Watch broadcast ROUND-ID (at URL) in a grid view."
  (interactive "sRound ID: \nsURL: ")
  (let* ((name (format "*Lichess Broadcast: %s*" round-id))
         (buf (get-buffer-create name)))
    (with-current-buffer buf
      (lichess-broadcast-mode)
      (setq lichess-broadcast--round-id round-id)
      (setq lichess-broadcast--round-url url)
      (lichess-broadcast--fetch-update))
    (pop-to-buffer buf)))

(defun lichess-broadcast--fetch-update ()
  "Fetch round data and schedule next update."
  (let ((round-id lichess-broadcast--round-id)
        (url lichess-broadcast--round-url)
        (buf (current-buffer)))
    (when (and round-id (buffer-live-p buf))
      (let ((primary-path
             (if (and url (string-match "lichess.org/\\(.*\\)" url))
                 (concat "/api/" (match-string 1 url))
               (format "/api/broadcast/round/%s" round-id))))

        (lichess-http-json
         primary-path
         (lambda (res)
           (let ((status (car res)))
             (if (and (/= status 200)
                      url
                      (not
                       (string=
                        primary-path
                        (format "/api/broadcast/round/%s" round-id))))
                 ;; Fallback to short path if primary failed and was different
                 (let ((fallback-path
                        (format "/api/broadcast/round/%s" round-id)))
                   (message
                    "Broadcast fetch failed (%s), retrying with: %s"
                    status fallback-path)
                   (lichess-http-json
                    fallback-path
                    (lambda (res2)
                      (lichess-broadcast--handle-update
                       res2 round-id))
                    nil t)) ;; retry anonymous
               ;; No fallback needed or already used
               (lichess-broadcast--handle-update res round-id))))
         nil t) ;; primary anonymous

        (setq lichess-broadcast--timer
              (run-at-time
               5 nil #'lichess-broadcast--fetch-update))))))

(defun lichess-broadcast--handle-update (res round-id)
  "Handle round update JSON RES for ROUND-ID."
  (let ((status (car res))
        (data (cdr res)))
    (let ((buf
           (get-buffer (format "*Lichess Broadcast: %s*" round-id))))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let ((inhibit-read-only t)
                (pt (point)))
            (erase-buffer)
            (if (/= status 200)
                (insert
                 (format
                  "Error fetching broadcast round %s: HTTP %s\n\nEnsure this round is accessible via API."
                  round-id status))
              (let* ((games
                      (append (lichess-util--aget data 'games) nil)))
                (message "Broadcast received %d games" (length games))
                (lichess-broadcast--render-grid games)))
            (goto-char pt)))))))

(defun lichess-broadcast--render-grid (games)
  "Render GAMES in a grid layout."
  (if (null games)
      (insert "No games in this round.")
    (let*
        ((width (window-width))
         ;; Estimate board width: rank/file labels + board + margins ~= 20-25 chars
         ;; Let's assume ~30 chars per column for safety in TUI
         (col-width 40)
         (cols (max 1 (/ width col-width)))
         (rows (/ (+ (length games) cols -1) cols)))

      (insert
       (format "Round Status: %d games active\n\n" (length games)))

      (dotimes (r rows)
        (let ((row-games (seq-take (seq-drop games (* r cols)) cols))
              (max-lines 0)
              (rendered-blocks '()))

          ;; 1. Render each game in the row to a list of strings
          (dolist (g row-games)
            (push (lichess-broadcast--render-game-block g col-width)
                  rendered-blocks))
          (setq rendered-blocks (nreverse rendered-blocks))

          ;; 2. Determine max height (lines)
          (setq max-lines
                (apply #'max (mapcar #'length rendered-blocks)))

          ;; 3. Print line by line
          (dotimes (L max-lines)
            (dolist (block rendered-blocks)
              (let ((line (or (nth L block) "")))
                (insert (format (format "%%-%ds  " col-width) line))))
            (insert "\n"))

          (insert "\n"))))))

(defun lichess-broadcast--render-game-block (game width)
  "Render a single GAME to a list of lines, constrained to WIDTH.
Returns a list of strings (lines) to be displayed."
  (let*
      ((players (append (lichess-util--aget game 'players) nil)) ;; Ensure list for safely accessing nth
       (white (nth 0 players))
       (black (nth 1 players))
       (fen
        (or
         (lichess-util--aget game 'fen)
         "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))
       (pos
        (condition-case nil
            (lichess-fen-parse fen)
          (error
           nil)))
       (w-name (or (lichess-util--aget white 'name) "White"))
       (b-name (or (lichess-util--aget black 'name) "Black"))
       (header
        (substring (format "%s vs %s" w-name b-name)
                   0
                   (min width (+ (length w-name) (length b-name) 4))))
       ;; Check preferred style
       (svg-p
        (and (boundp 'lichess-board-gui-preferred-style)
             (string= lichess-board-gui-preferred-style "svg")
             (lichess-board-gui-available-p))))
    (if (and svg-p pos)
        ;; SVG Mode: Return header + 1 line containing the image
        (let ((svg-str
               (lichess-board-gui-draw
                pos
                (if (eq (lichess-pos-stm pos) 'b)
                    'black
                  'white))))
          (list header svg-str))
      ;; TUI Mode
      (let* ((board-str
              (if pos
                  (lichess-board-tui-draw pos "unicode" 'from-stm)
                "[Invalid Position]"))
             (lines (split-string board-str "\n")))
        (cons header lines)))))

(provide 'lichess-broadcast-view)
;;; lichess-broadcast-view.el ends here
