;;; lichess-game.el --- Game stream/debug utilities for Lichess -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025  Alexandr Timchenko
;; SPDX-License-Identifier: GPL-3.0-or-later
;; See LICENSE for details.
;;
;;; Commentary:
;; Minimal game streaming/debug using NDJSON:
;; - M-x lichess-game-stream-debug  → pretty-print /api/stream/game/{id}
;; - M-x lichess-game-stream-stop   → close the current stream
;;
;;; Code:

(require 'cl-lib)
(require 'lichess-core)   ;; pulls in lichess-http as a dependency

(defvar lichess-game--stream nil
  "Current `lichess-http-stream' for a game NDJSON, or nil when not running.")

(defun lichess-game--stream-buffer-name (id)
  (format "*Lichess Game Stream: %s*" id))

(defvar lichess-game--debug-id "t7HAF0vX"
  "Default game ID used for debugging.")

;;;###autoload
(defun lichess-game-stream-debug (id)
  "Open NDJSON stream for game ID at /api/stream/game/{id} and pretty-print events."
  (interactive
   (list
    (read-string
     (format "Lichess game id (default %s): " lichess-game--debug-id)
     nil nil lichess-game--debug-id)))
  (let* ((buf (get-buffer-create (lichess-game--stream-buffer-name id))))
    (with-current-buffer buf (lichess-core-mode))
    ;; Close previous stream if any
    (when lichess-game--stream
      (lichess-http-ndjson-close lichess-game--stream)
      (setq lichess-game--stream nil))
    ;; Open a new NDJSON stream via lichess-http
    (setq lichess-game--stream
          (lichess-http-ndjson-open
           (format "/api/stream/game/%s" id)
           :buffer-name (buffer-name buf)
           :on-open (lambda (_proc _buf)
                      (lichess-core-with-buf buf
                        (erase-buffer)
                        (insert (format "Connecting NDJSON stream for game %s…\n\n" id))))
           :on-event (lambda (obj)
                       (lichess-core-with-buf buf
                         (goto-char (point-max))
                         (insert (format "[%s] — NDJSON event\n"
                                         (format-time-string "%H:%M:%S")))
                         (pp obj (current-buffer))
                         (insert "\n")))
           :on-close (lambda (_proc msg)
                       (lichess-core-with-buf buf
                         (goto-char (point-max))
                         (insert (format "[%s] — %s\n"
                                         (format-time-string "%H:%M:%S")
                                         (string-trim msg)))))))
    (pop-to-buffer buf)))

;;;###autoload
(defun lichess-game-stream-stop ()
  "Stop the current Lichess NDJSON game stream (if any)."
  (interactive)
  (if lichess-game--stream
      (progn
        (lichess-http-ndjson-close lichess-game--stream)
        (setq lichess-game--stream nil)
        (message "Lichess NDJSON stream stopped."))
    (message "No active Lichess NDJSON stream.")))

(provide 'lichess-game)
;;; lichess-game.el ends here
