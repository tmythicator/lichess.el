;;; lichess.el --- Client for Lichess.org -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026  Alexandr Timchenko
;;
;; Author: Alexandr Timchenko <atimchenko92@gmail.com>
;; Maintainer: Alexandr Timchenko <atimchenko92@gmail.com>
;; URL: https://github.com/tmythicator/Lichess.el
;; Version: 0.6
;; Package-Requires: ((emacs "27.1"))
;; Keywords: games, chess, lichess, api
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is part of Lichess.el.
;;
;; Lichess.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; See the LICENSE file or <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Lichess.el is an Emacs client for the https://lichess.org API.
;;
;; This file is a main entry point for the Lichess.el client.
;;
;; Features:
;; - Play against AI
;; - Watch Lichess TV and arbitrary games
;; - Board rendering styles: GUI (SVG), Unicode, and ASCII
;;
;; Run `M-x lichess` to open the main menu, which provides access to:
;; - `lichess-tv`: Watch ongoing top games on Lichess TV
;; - `lichess-game-watch`: Watch a specific game by ID
;; - `lichess-ai-challenge`: Play a game against Stockfish
;; - `lichess-set-style`: Configure board visualization (GUI/Unicode/ASCII)
;;
;;; Code:

(require 'lichess-core)
(require 'lichess-tv)
(require 'lichess-game)
(require 'lichess-ai)
(require 'lichess-fen)
(require 'lichess-board)
(require 'lichess-board-tui)


(defgroup lichess nil
  "Lichess client."
  :group 'applications)

(defcustom lichess-token nil
  "Personal Lichess API token."
  :type 'string
  :group 'lichess)

;;;###autoload
(defun lichess ()
  "Dispatch Lichess commands."
  (interactive)
  (let* ((base-choices
          '(("Lichess TV: Show channels" . lichess-tv)
            ("Lichess: Watch game" . lichess-game-watch)
            ("Lichess: Play against AI" . lichess-ai-challenge)
            ("Settings: Set Board Style" . lichess-set-style)))
         (debug-choices
          (if (locate-library "lichess-debug")
              '(("(Debug) Diagnose account" . lichess-debug-diagnose)
                ("(Debug) NDJSON Stream" . lichess-debug-game-stream)
                ("(Debug) TV: JSON channels" . lichess-debug-tv)
                ("(Debug) Render FEN position" . lichess-fen-show))
            nil))
         (choices (append base-choices debug-choices))
         (pick
          (completing-read "Lichess: " (mapcar #'car choices) nil t))
         (cmd (cdr (assoc pick choices))))
    (when (memq
           cmd
           '(lichess-debug-diagnose
             lichess-debug-game-stream lichess-debug-tv))
      (require 'lichess-debug))
    (call-interactively cmd)))

;;;###autoload
(defun lichess-set-style (style)
  "Set the board rendering STYLE interactively.
STYLE: \"unicode\", \"ascii\", or \"svg\".
Warns if \"svg\" is selected but unavailable or missing assets."
  (interactive (list
                (completing-read
                 "Board Style: " '("unicode" "ascii" "svg")
                 nil t)))

  (when (string= style "svg")
    (cond
     ((not (display-graphic-p))
      (display-warning
       'lichess
       "SVG style selected but Emacs is not running in graphical mode. Fallback to TUI will apply."
       :warning))
     ((not (lichess-board-gui-available-p))
      (display-warning
       'lichess
       "SVG style selected but SVG support is missing in this Emacs build."
       :warning))
     (t
      (let ((missing (lichess-board-gui-missing-assets)))
        (when missing
          (display-warning
           'lichess
           (format "SVG style selected but assets are missing: %s"
                   (string-join missing ", "))
           :warning))))))

  (if (display-graphic-p)
      (customize-set-variable
       'lichess-board-gui-preferred-style style)
    (customize-set-variable 'lichess-board-tui-preferred-style style))
  (message "Lichess Board Style set to '%s' (%s)"
           style
           (if (display-graphic-p)
               "GUI"
             "TUI"))

  ;; Refresh open buffers
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (cond
       ((and (boundp 'lichess-fen--current-pos)
             lichess-fen--current-pos
             (fboundp 'lichess-fen-refresh))
        (funcall #'lichess-fen-refresh))

       ((and (boundp 'lichess-game--current-idx)
             lichess-game--current-idx
             (fboundp 'lichess-game-refresh))
        (funcall #'lichess-game-refresh))))))

(provide 'lichess)
;;; lichess.el ends here
