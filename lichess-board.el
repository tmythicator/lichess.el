;;; lichess-board.el --- Board rendering dispatcher -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026  Alexandr Timchenko
;; URL: https://github.com/tmythicator/Lichess.el
;; Version: 0.9
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Interface for rendering position structs.
;; Dispatches to TUI or GUI implementation.

;;; Code:

(require 'lichess-board-tui)
(require 'lichess-board-gui)

(defcustom lichess-board-gui-preferred-style "svg"
  "Preferred board rendering style in GUI environments.
Values: \"svg\", \"unicode\", \"ascii\"."
  :type '(choice (const "svg") (const "unicode") (const "ascii"))
  :group 'lichess)

(defcustom lichess-board-tui-preferred-style "unicode"
  "Preferred board rendering style in Terminal environments.
Values: \"unicode\", \"ascii\"."
  :type '(choice (const "unicode") (const "ascii"))
  :group 'lichess)

(defun lichess-board--active-style ()
  "Return active style string based on environment capability and preferences."
  (if (lichess-board-gui-available-p)
      (or lichess-board-gui-preferred-style "svg")
    (or lichess-board-tui-preferred-style "unicode")))

(defun lichess-board-draw (pos &optional perspective highlights)
  "Render POS as a string.
PERSPECTIVE: \`white', \`black', \`auto'.
HIGHLIGHTS: List of squares to highlight.
EVAL and INFO are read from POS."
  (let ((style (lichess-board--active-style)))
    (if (string= style "svg")
        (lichess-board-gui-draw
         pos perspective highlights (plist-get pos :eval))
      (lichess-board-tui-draw pos style perspective))))

(defun lichess-board-draw-heading (pos &optional perspective)
  "Render heading string for POS using global style and PERSPECTIVE."
  (let* ((style (lichess-board--active-style))
         (display-style
          (if (string= style "svg")
              "SVG"
            style)))
    (lichess-board-tui-draw-heading pos display-style perspective)))

(defun lichess-board-insert-board
    (pos &optional perspective highlights)
  "Insert the board rendering POS into the current buffer at point.
PERSPECTIVE: `white`, `black`, or `auto`.
HIGHLIGHTS: List of squares to highlight.
Handles face application for TUI modes, avoiding interference with GUI SVGs."
  (let ((style (lichess-board--active-style))
        (start (point)))
    (insert (lichess-board-draw pos perspective highlights))
    (unless (string= style "svg")
      (add-text-properties
       start (point) '(face lichess-core-board-face)))))

(defun lichess-board-render-to-buffer
    (pos &optional perspective highlights preamble postamble)
  "Clear and render POS to the current buffer.
Standardizes rendering for both game and FEN views.
- POS: `lichess-pos` struct.
- PERSPECTIVE: `white`, `black`, or `auto`.
- HIGHLIGHTS: List of squares to highlight.
- PREAMBLE: Optional text to insert at the very top.
- POSTAMBLE: Optional text to insert below the board.
Uses `eval` and `info` from POS if present."
  (let ((inhibit-read-only t))
    (erase-buffer)

    ;; 0. Preamble (Names, clocks, etc.)
    (when preamble
      (insert preamble "\n"))

    ;; 1. Heading
    (insert (lichess-board-draw-heading pos perspective))

    ;; 2. Board
    (lichess-board-insert-board pos perspective highlights)
    (insert "\n")

    ;; 3. Postamble
    (when postamble
      (insert "\n" postamble))

    ;; 4. Info/Footer
    (when (plist-get pos :info)
      (insert "\n" (plist-get pos :info)))

    ;; 5. Reset cursor
    (goto-char (point-min))))

(provide 'lichess-board)
;;; lichess-board.el ends here
