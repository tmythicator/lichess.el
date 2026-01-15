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

(provide 'lichess-board)
;;; lichess-board.el ends here
