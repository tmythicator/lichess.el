;;; lichess-board.el --- Board rendering dispatcher -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026  Alexandr Timchenko
;; URL: https://github.com/tmythicator/Lichess.el
;; Version: 0.4
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Interface for rendering position structs.
;; Dispatches to TUI or GUI implementation.

;;; Code:

(require 'lichess-board-tui)
(require 'lichess-board-gui)

(defcustom lichess-board-gui-preferred-style "gui"
  "Preferred board rendering style in GUI environments.
Values: \"gui\", \"unicode\", \"ascii\"."
  :type '(choice (const "gui") (const "unicode") (const "ascii"))
  :group 'lichess)

(defcustom lichess-board-tui-preferred-style "unicode"
  "Preferred board rendering style in Terminal environments.
Values: \"unicode\", \"ascii\"."
  :type '(choice (const "unicode") (const "ascii"))
  :group 'lichess)

(defun lichess-board-draw
    (pos &optional perspective eval-str highlights)
  "Render POS as a string.
PERSPECTIVE: \`white', \`black', \`auto'.
EVAL-STR: Optional evaluation string.
HIGHLIGHTS: List of squares to highlight."
  (let* ((gui-avail (lichess-board-gui-available-p))
         (style
          (if gui-avail
              lichess-board-gui-preferred-style
            lichess-board-tui-preferred-style)))
    (if (and (string= style "gui") gui-avail)
        (lichess-board-gui-draw pos perspective highlights)
      (let ((tui-style
             (if (string= style "gui")
                 "unicode"
               style)))
        (lichess-board-tui-draw
         pos tui-style perspective eval-str)))))

(defun lichess-board-draw-heading (pos &optional perspective)
  "Render heading string for POS using global style and PERSPECTIVE."
  (let* ((gui-avail (lichess-board-gui-available-p))
         (style
          (if gui-avail
              lichess-board-gui-preferred-style
            lichess-board-tui-preferred-style))
         (display-style
          (if (and gui-avail (string= style "gui"))
              "GUI"
            (if (string= style "gui")
                "unicode"
              style))))
    (lichess-board-tui-draw-heading pos display-style perspective)))

(defun lichess-board-insert-board
    (pos &optional perspective eval-str highlights)
  "Insert the board rendering POS into the current buffer at point.
PERSPECTIVE: `white`, `black`, or `auto`.
EVAL-STR: Optional evaluation string.
HIGHLIGHTS: List of squares to highlight.
Handles face application for TUI modes, avoiding interference with GUI SVGs."
  (let* ((gui-avail (lichess-board-gui-available-p))
         (style
          (if gui-avail
              lichess-board-gui-preferred-style
            lichess-board-tui-preferred-style))
         (gui-p (and gui-avail (string= style "gui")))
         (board-str
          (if gui-p
              (lichess-board-gui-draw pos perspective highlights)
            (let ((tui-style
                   (if (string= style "gui")
                       "unicode"
                     style)))
              (lichess-board-tui-draw
               pos tui-style perspective eval-str))))
         (start (point)))
    (insert board-str)
    (unless gui-p
      (add-text-properties
       start (point) '(face lichess-core-board-face)))))

(defun lichess-board-render-to-buffer
    (pos &optional perspective eval-str info-str highlights preamble)
  "Clear and render POS to the current buffer.
Standardizes rendering for both game and FEN views.
- POS: `lichess-pos` struct.
- PERSPECTIVE: `white`, `black`, or `auto`.
- EVAL-STR: Optional evaluation string.
- INFO-STR: Optional extra text to append.
- HIGHLIGHTS: List of squares to highlight.
- PREAMBLE: Optional text to insert at the very top."
  (let ((inhibit-read-only t))
    (erase-buffer)

    ;; 0. Preamble (Names, clocks, etc.)
    (when preamble
      (insert preamble "\n"))

    ;; 1. Heading
    (insert (lichess-board-draw-heading pos perspective))

    ;; 2. Board
    (lichess-board-insert-board pos perspective eval-str highlights)
    (insert "\n")

    ;; 3. Info/Footer
    (when info-str
      (insert "\n" info-str))

    ;; 4. Reset cursor
    (goto-char (point-min))))

(provide 'lichess-board)
;;; lichess-board.el ends here
