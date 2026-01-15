;;; lichess-board.el --- Board rendering dispatcher -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026  Alexandr Timchenko
;; URL: https://github.com/tmythicator/Lichess.el
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Interface for rendering position structs.
;; Dispatches to TUI or GUI implementation.

;;; Code:

(require 'lichess-board-tui)
(require 'lichess-board-gui)

(defcustom lichess-board-style "gui"
  "Preferred board rendering style.
Values: \"unicode\", \"ascii\", \"gui\"."
  :type '(choice (const "unicode") (const "ascii") (const "gui"))
  :group 'lichess)

(defun lichess-board-draw (pos &optional perspective eval-str)
  "Render POS as a string.
PERSPECTIVE: \`white', \`black', \`auto'.
EVAL-STR: Optional evaluation string."
  (let ((s lichess-board-style))
    (if (and (string= s "gui")
             (fboundp 'lichess-board-gui-available-p)
             (lichess-board-gui-available-p))
        (lichess-board-gui-draw pos perspective)
      ;; Force TUI fallback if GUI requested but unavailable
      (let ((tui-style
             (if (string= s "gui")
                 "unicode"
               s)))
        (lichess-board-tui-draw
         pos tui-style perspective eval-str)))))

(defun lichess-board-draw-heading (pos &optional perspective)
  "Render heading string for POS using global style and PERSPECTIVE."
  (let* ((s lichess-board-style)
         (tui-style
          (if (string= s "gui")
              "unicode"
            s)))
    (lichess-board-tui-draw-heading pos tui-style perspective)))

(defun lichess-board-insert-board (pos &optional perspective eval-str)
  "Insert the board rendering POS into the current buffer at point.
PERSPECTIVE: `white`, `black`, or `auto`.
EVAL-STR: Optional evaluation string.
Handles face application for TUI modes, avoiding interference with GUI SVGs."
  (let* ((s lichess-board-style)
         (gui-p
          (and (string= s "gui")
               (fboundp 'lichess-board-gui-available-p)
               (lichess-board-gui-available-p)))
         (board-str
          (if gui-p
              (lichess-board-gui-draw pos perspective)
            (let ((tui-style
                   (if (string= s "gui")
                       "unicode"
                     s)))
              (lichess-board-tui-draw
               pos tui-style perspective eval-str))))
         (start (point)))
    (insert board-str)
    (unless gui-p
      (add-text-properties
       start (point) '(face lichess-core-board-face)))))

(defun lichess-board-render-to-buffer
    (pos &optional perspective eval-str info-str)
  "Clear and render POS to the current buffer.
Standardizes rendering for both game and FEN views.
- POS: `lichess-pos` struct.
- PERSPECTIVE: `white`, `black`, or `auto`.
- EVAL-STR: Optional evaluation string.
- INFO-STR: Optional extra text to append."
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; 1. Heading
    (insert (lichess-board-draw-heading pos perspective))

    ;; 2. Board
    (lichess-board-insert-board pos perspective eval-str)
    (insert "\n")

    ;; 3. Info/Footer
    (when info-str
      (insert "\n" info-str))

    ;; 4. Reset cursor
    (goto-char (point-min))))

(provide 'lichess-board)
;;; lichess-board.el ends here
