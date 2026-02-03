;;; lichess-broadcast-view.el --- Watch Lichess Broadcast -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026  Alexandr Timchenko
;; URL: https://github.com/tmythicator/Lichess.el
;; Version: 0.8
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
(require 'lichess-api)
(require 'lichess-fen)
(require 'lichess-board-gui)
(require 'lichess-board-tui)
(require 'subr-x)
(require 'seq)
(require 'cl-lib)

;; I decided to go Clojure-style with this one.
(defvar-local lichess-broadcast-view--state nil
  "The broadcast view state plist for the current buffer.
Properties:
  :round-id  (string)  The unique ID of the round being watched.
  :url       (string)  The API URL for updates.
  :timer     (timer)   The active auto-update timer object.
  :games     (list)    List of game objects from the last update.")

(defvar lichess-broadcast-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `lichess-broadcast-view-mode'.")

(define-derived-mode
 lichess-broadcast-view-mode
 lichess-core-mode
 "Lichess-Broadcast"
 "Mode for watching a live broadcast round."
 (setq truncate-lines t)
 (add-hook 'kill-buffer-hook #'lichess-broadcast-view--cleanup-timer
           nil t))

(defun lichess-broadcast-view--cleanup-timer ()
  "Stop the broadcast timer."
  (when-let* ((timer
               (plist-get lichess-broadcast-view--state :timer)))
    (cancel-timer timer)
    (setq lichess-broadcast-view--state
          (plist-put lichess-broadcast-view--state :timer nil))))

(cl-defun
 lichess-broadcast-view-watch-round
 (round-id &optional url)
 "Watch broadcast ROUND-ID (at URL) in a grid view."
 (interactive "sRound ID: \nsURL: ")
 (let* ((name (format "*Lichess Broadcast: %s*" round-id))
        (buf (get-buffer-create name)))
   (with-current-buffer buf
     (lichess-broadcast-view-mode)

     ;; Initialize state map
     (setq lichess-broadcast-view--state
           (list :round-id round-id :url url))
     (lichess-broadcast-view--fetch-update))
   (pop-to-buffer buf)))

(defun lichess-broadcast-view--fetch-update ()
  "Fetch round data and schedule next update."
  (when-let* ((round-id
               (plist-get lichess-broadcast-view--state :round-id))
              (url (plist-get lichess-broadcast-view--state :url))
              (buf (current-buffer))
              (_ (buffer-live-p buf)))

    (lichess-api-get-broadcast-round
     url
     (lambda (res)
       (lichess-broadcast-view--handle-update res round-id)))

    ;; Schedule next update in state
    (setq lichess-broadcast-view--state
          (plist-put
           lichess-broadcast-view--state
           :timer
           (run-at-time
            5 nil #'lichess-broadcast-view--fetch-update)))))

(defun lichess-broadcast-view--handle-update (res round-id)
  "Handle round update JSON RES for ROUND-ID."
  (pcase-let* ((`(,status . ,data) res)
               (buf
                (get-buffer
                 (format "*Lichess Broadcast: %s*" round-id))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (pt (point)))
          (erase-buffer)
          (if (/= status 200)
              (insert
               (format
                "Error fetching broadcast %s: %s\n\nEnsure accessible via API."
                round-id status))
            (let ((games
                   (append (lichess-util--aget data 'games) nil)))
              ;; Update state map
              (setq lichess-broadcast-view--state
                    (plist-put
                     lichess-broadcast-view--state
                     :games games))
              (message "Broadcast received %d games" (length games))
              (lichess-broadcast-view--render-grid games)))
          (goto-char pt))))))

(defun lichess-broadcast-view--render-grid (games)
  "Render GAMES in a grid layout."
  (if (null games)
      (insert "No games in this round.")

    (let* ((width (window-width))
           (col-width 40)
           (cols (max 1 (/ width col-width))))

      (insert
       (format "Round Status: %d games active\n\n" (length games)))

      (seq-do
       (lambda (row)
         (lichess-broadcast-view--render-row row col-width))
       (seq-partition games cols)))))

(defun lichess-broadcast-view--render-row (row-games col-width)
  "Render and insert a row of ROW-GAMES.
Each game is rendered to a block of lines, then those blocks are
joined side-by-side using COL-WIDTH spacing."
  (let* ((blocks
          (mapcar
           (lambda (g)
             (lichess-broadcast-view--render-game-block g col-width))
           row-games))
         (height (apply #'max (mapcar #'length blocks))))

    (dotimes (i height)
      (dolist (block blocks)
        (let ((line (or (nth i block) "")))
          (insert (format (format "%%-%ds  " col-width) line))))
      (insert "\n"))
    (insert "\n")))

(cl-defun
 lichess-broadcast-view--render-game-block
 (game width &key (style :auto))
 "Render a single GAME to a list of lines, constrained to WIDTH.
Optional STYLE argument can force :svg or :tui."
 (pcase-let*
     ((`(,white ,black)
       (append (lichess-util--aget game 'players) nil))
      (fen
       (or
        (lichess-util--aget game 'fen)
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))
      (pos
       (condition-case nil
           (lichess-fen-parse fen)
         (error
          nil)))
      (w-name (lichess-util-fmt-user-obj white))
      (b-name (lichess-util-fmt-user-obj black))
      (header
       (substring (format "%s vs %s" w-name b-name)
                  0
                  (min width (+ (length w-name) (length b-name) 4))))
      (use-svg?
       (or (eq style :svg)
           (and (eq style :auto)
                (boundp 'lichess-board-gui-preferred-style)
                (string= lichess-board-gui-preferred-style "svg")
                (lichess-board-gui-available-p)))))

   (if (and use-svg? pos)
       (let ((svg-str
              (lichess-board-gui-draw
               pos
               (if (eq (plist-get pos :stm) 'b)
                   'black
                 'white))))
         (list header svg-str))

     (let* ((board-str
             (if pos
                 (lichess-board-tui-draw pos "unicode" 'from-stm)
               "[Invalid Position]"))
            (lines (split-string board-str "\n")))
       (cons header lines)))))

(provide 'lichess-broadcast-view)
;;; lichess-broadcast-view.el ends here
