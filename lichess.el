;;; lichess.el --- Lichess client -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026  Alexandr Timchenko
;;
;; Author: Alexandr Timchenko <atimchenko92@gmail.com>
;; Maintainer: Alexandr Timchenko <atimchenko92@gmail.com>
;; URL: https://github.com/tmythicator/Lichess.el
;; Version: 0.1
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
;; Lichess.el is a modular Emacs client for the https://lichess.org API.
;;
;; This file is a main entry point for the modular Lichess.el client.
;;
;; Provides a dispatcher (`M-x lichess`) to access all submodules:
;; - Auth diagnostics (`M-x lichess-diagnose`)
;; - Lichess TV (`M-x lichess-tv`)
;; - Game preview (=M-x lichess-game-preview-fen=)
;;
;;; Code:

(require 'lichess-core)
(require 'lichess-tv)
(require 'lichess-game)
(require 'lichess-ai)
(require 'lichess-fen)

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
  (let* ((choices
          '(("Lichess TV: Show channels" . lichess-tv)
            ("Lichess: Watch game" . lichess-game-watch)
            ("Lichess: Play against AI" . lichess-ai-challenge)
            ("(Debug) Diagnose account" . lichess-debug-diagnose)
            ("(Debug) NDJSON Stream" . lichess-debug-game-stream)
            ("(Debug) TV: JSON channels" . lichess-debug-tv)
            ("(Debug) Render FEN position" . lichess-fen-show)))
         (pick
          (completing-read "Lichess: " (mapcar #'car choices) nil t))
         (cmd (cdr (assoc pick choices))))
    (when (memq
           cmd
           '(lichess-debug-diagnose
             lichess-debug-game-stream lichess-debug-tv))
      (require 'lichess-debug))
    (call-interactively cmd)))

(provide 'lichess)
;;; lichess.el ends here
