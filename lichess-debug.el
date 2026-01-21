;;; lichess-debug.el --- Debugging tools for Lichess.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026  Alexandr Timchenko
;; URL: https://github.com/tmythicator/Lichess.el
;; Version: 0.6
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; See LICENSE for details.
;;
;;; Commentary:
;;
;; Debugging commands and diagnostics for development.
;;
;;; Code:

(require 'lichess-core)
(require 'lichess-http)
(require 'lichess-game)
(require 'lichess-tv)
(require 'lichess-util)

(defvar lichess-debug-diagnose-buf "*Lichess Diagnose*")
(defvar lichess-debug-tv-buf "*(Debug) Lichess TV*")
(defvar lichess-debug-game-id "t7HAF0vX"
  "Default game ID used for debugging.")

(defun lichess-debug--log (fmt &rest args)
  "Append FMT line to `lichess-debug-diagnose-buf' safely.
ARGS are passed to `format`."
  (let ((buf (get-buffer-create lichess-debug-diagnose-buf)))
    (with-current-buffer buf
      (lichess-core-mode))
    (lichess-core-with-buf
     buf
     (goto-char (point-max))
     (insert (apply #'format fmt args) "\n"))
    (pop-to-buffer buf)))

;;;###autoload
(defun lichess-debug-diagnose ()
  "Check auth and show /account + /account/playing."
  (interactive)
  (lichess-debug--log "Lichess diagnostics\n")
  (if (not
       (and (boundp 'lichess-token)
            (stringp lichess-token)
            (> (length lichess-token) 10)))
      (lichess-debug--log
       "Set lichess-token for authenticated calls.")
    (lichess-debug--log "/api/account …")
    (lichess-http-json
     "/api/account"
     (lambda (res)
       (if (/= (car res) 200)
           (lichess-debug--log "HTTP %s /account" (car res))
         (let ((j (cdr res)))
           (lichess-debug--log "Auth OK as %s"
                               (alist-get 'username j))))
       (lichess-debug--log "/api/account/playing …")
       (lichess-http-json
        "/api/account/playing"
        (lambda (res2)
          (pcase (car res2)
            (200 (let* ((j (cdr res2))
                        (games (alist-get 'nowPlaying j))
                        (n (length games)))
                   (lichess-debug--log (if (> n 0)
                                           "%d ongoing game(s)"
                                         "nowPlaying = []")
                                       n)))
            (_
             (lichess-debug--log "HTTP %s /account/playing"
                                 (car res2))))))))))

;;;###autoload
(defun lichess-debug-game-stream (id)
  "Open NDJSON stream for game ID at /api/stream/game/{id} and pretty-print events."
  (interactive (list
                (read-string (format "Lichess game id (default %s): "
                                     lichess-debug-game-id)
                             nil nil lichess-debug-game-id)))
  (let* ((buf
          (get-buffer-create
           (format "*Lichess Game Stream: %s*" id))))
    (with-current-buffer buf
      (lichess-core-mode))
    ;; Close previous stream if any (we can't easily access the let-bound one in lichess-game,
    ;; but normally debug is separate. If we want to share state we'd need to expose it.)
    ;; For debug, we'll just open a new one.

    (lichess-http-ndjson-open
     (format "/api/stream/game/%s" id)
     :buffer-name (buffer-name buf)
     :on-open
     (lambda (_proc _buf)
       (lichess-core-with-buf
        buf (erase-buffer)
        (insert
         (format "Connecting NDJSON stream for game %s…\n\n" id))))
     :on-event
     (lambda (obj)
       (lichess-core-with-buf
        buf (goto-char (point-max))
        (insert
         (format "[%s] — NDJSON event\n" (format-time-string "%T")))
        (pp obj (current-buffer)) (insert "\n")))
     :on-close
     (lambda (_proc msg)
       (lichess-core-with-buf
        buf (goto-char (point-max))
        (insert
         (format "[%s] — %s\n"
                 (format-time-string "%T") (string-trim msg))))))
    (pop-to-buffer buf)))

;;;###autoload
(defun lichess-debug-tv (&optional channel)
  "Dump raw JSON for /api/tv/channels and (optionally) /api/game/{id} of CHANNEL."
  (interactive (list
                (let* ((chs
                        '("best"
                          "blitz"
                          "bullet"
                          "rapid"
                          "classical"
                          "bot"
                          "computer"
                          "racingKings"
                          "crazyhouse"
                          "threeCheck"
                          "kingOfTheHill"
                          "atomic"
                          "horde"
                          "chess960"
                          "ultraBullet"))
                       (ans
                        (completing-read "Channel (empty = all): " chs
                                         nil nil "")))
                  (if (string-empty-p ans)
                      nil
                    ans))))
  (let* ((buf (get-buffer-create lichess-debug-tv-buf))
         tail)
    (with-current-buffer buf
      (lichess-tv-mode))
    (lichess-core-with-buf
     buf
     (erase-buffer)
     (insert "Fetching /api/tv/channels …\n\n")
     (setq tail (copy-marker (point-max) t)))
    (pop-to-buffer buf)
    (cl-labels
     ((append-tail
       (&rest xs)
       (lichess-core-with-buf
        buf (goto-char (marker-position tail))
        (while xs
          (let ((x (pop xs)))
            (cond
             ((eq x :nl)
              (insert "\n"))
             ((eq x :hr)
              (insert (make-string 70 ?─) "\n"))
             ((eq x :ts)
              (insert (format-time-string "[%T] ")))
             ((eq x :pp)
              (pp (pop xs) (current-buffer)))
             ((stringp x)
              (insert x))
             (t
              (insert (format "%s" x))))))
        (insert "\n") (setq tail (copy-marker (point) t)))))
     ;; Get channels
     (lichess-http-json
      "/api/tv/channels"
      (lambda (res)
        (let ((status (car res))
              (data (cdr res)))
          (append-tail
           :hr
           :ts (format "HTTP %s /api/tv/channels" status)
           :nl "--- RAW JSON ---"
           :nl
           :nl
           :pp data
           :hr
           :nl)
          ;; Get optional game of CHANNEL
          (when (and (= status 200) channel)
            (let* ((sym (intern (downcase channel)))
                   (entry (alist-get sym data))
                   (gid
                    (and entry
                         (or (alist-get 'gameId entry)
                             (alist-get 'id entry)))))
              (append-tail
               (format "Channel %s -> game %s" channel (or gid "nil"))
               :nl
               :hr
               :nl)
              (when gid
                (lichess-http-json
                 (format "/api/game/%s" gid)
                 (lambda (res2)
                   (append-tail
                    :ts
                    (format "HTTP %s /api/game/%s" (car res2) gid)
                    :nl "--- RAW JSON ---"
                    :nl
                    :nl
                    :pp (cdr res2)
                    :hr
                    :nl))))))))))))

(provide 'lichess-debug)
;;; lichess-debug.el ends here
