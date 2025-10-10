;;; lichess.el --- Lichess client for Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025  Alexandr Timchenko
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
;; - Raw API inspection (`M-x lichess-tv-debug`)
;;
;;; Code:

(require 'lichess-core)
(require 'lichess-tv)
(require 'lichess-game)
(require 'lichess-fen)

(defcustom lichess-token nil
  "Personal Lichess API token."
  :type 'string :group 'lichess)

(defvar lichess-diagnose--buf "*Lichess Diagnose*")
(defun lichess--dbgln (fmt &rest args)
  "Append FMT line to 'lichess-diagnose--buf' safely."
  (let ((buf (get-buffer lichess-diagnose--buf)))
    (when (buffer-live-p buf)
      (lichess-core-with-buf buf
        (goto-char (point-max))
        (insert (apply #'format fmt args) "\n")))))

;;;###autoload
(defun lichess ()
  "Dispatch Lichess commands."
  (interactive)
  (let* ((choices '(("TV: channels"      . lichess-tv)
                    ("Diagnose account"  . lichess-diagnose)
                    ("(Debug) NDJSON Stream" . lichess-game-stream-debug)
                    ("(Debug) TV: JSON channels" . lichess-tv-debug)
                    ("(Debug) Render FEN position" . lichess-fen-show)))
         (pick (completing-read "Lichess: " (mapcar #'car choices) nil t)))
    (call-interactively (cdr (assoc pick choices)))))

;;;###autoload
(defun lichess-diagnose ()
  "Check auth and show /account + /account/playing."
  (interactive)
  (let ((buf (get-buffer-create lichess-diagnose--buf)))
    (with-current-buffer buf
      (lichess-core-mode))
    (lichess-core-with-buf buf
      (erase-buffer)
      (insert "Lichess diagnostics\n\n"))
    (pop-to-buffer buf))
  (if (not (and (stringp lichess-token) (> (length lichess-token) 10)))
      (lichess--dbgln "Set lichess-token for authenticated calls.")
    (lichess--dbgln "/api/account …")
    (lichess-http-json
     "/api/account"
     (lambda (res)
       (if (/= (car res) 200)
           (lichess--dbgln "HTTP %s /account" (car res))
         (let ((j (cdr res)))
           (lichess--dbgln "Auth OK as %s" (alist-get 'username j))))
       (lichess--dbgln "/api/account/playing …")
       (lichess-http-json
        "/api/account/playing"
        (lambda (res2)
          (pcase (car res2)
            (200 (let* ((j (cdr res2))
                        (games (alist-get 'nowPlaying j))
                        (n (length games)))
                   (lichess--dbgln (if (> n 0)
                                       "%d ongoing game(s)" "nowPlaying = []") n)))
            (_ (lichess--dbgln "HTTP %s /account/playing" (car res2))))))))))

(provide 'lichess)
;;; lichess.el ends here