;;; lichess-api.el --- Lichess API Endpoints -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026  Alexandr Timchenko
;; URL: https://github.com/tmythicator/Lichess.el
;; Version: 0.6
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; Centralized API definitions for Lichess.el.
;; All external API calls should be routed through this module.
;;
;;; Code:

(require 'lichess-http)

;;; TV
(defun lichess-api-get-tv-channels (callback)
  "Fetch TV channels.  CALLBACK received (STATUS . DATA)."
  (lichess-http-json "/api/tv/channels" callback))

;;; Broadcasts
(defun lichess-api-get-broadcasts (callback &optional nb)
  "Fetch top broadcasts.  CALLBACK is called with (STATUS . DATA).
NB is count (default 20)."
  (let ((n (or nb 20)))
    (lichess-http-json
     (format "/api/broadcast/top?nb=%d" n) callback)))

(defun lichess-api-get-broadcast-round (round-id callback)
  "Fetch broadcast round data for ROUND-ID.  CALLBACK: (STATUS . DATA)."
  (lichess-http-json
   (format "/api/broadcast/round/%s" round-id) callback))

(defun lichess-api-get-broadcast-round-url (round-id)
  "Return endpoint URL for broadcast ROUND-ID."
  (format "/api/broadcast/round/%s" round-id))

(defun lichess-api-get-game (game-id callback)
  "Fetch game data for GAME-ID.  CALLBACK: (STATUS . DATA)."
  (lichess-http-json (format "/api/game/%s" game-id) callback))

;;; Challenges / AI
(defun lichess-api-challenge-ai
    (level color limit increment fen callback text-mode)
  "Challenge AI.
LEVEL: 1-8.
COLOR: `white', `black', or `random'.
LIMIT: Clock limit in seconds.
INCREMENT: Clock increment in seconds.
FEN: Optional starting position.
CALLBACK: (STATUS . DATA).
TEXT-MODE: If non-nil, parse response as `raw' text."
  (let* ((params
          `(("level" ,(number-to-string level))
            ("color" ,(symbol-name color))
            ("clock.limit" ,(number-to-string limit))
            ("clock.increment" ,(number-to-string increment))))
         (final-params
          (if fen
              (cons `("fen" ,fen) params)
            params)))
    (lichess-http-request
     "/api/challenge/ai" callback
     :method "POST"
     :data (url-build-query-string final-params)
     :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
     :parse
     (if text-mode
         'raw
       'json))))

;;; Cloud Eval
(defun lichess-api-cloud-eval (fen callback)
  "Fetch cloud evaluation for FEN.
CALLBACK receives evaluation string or :unavailable."
  (let ((encoded-fen (url-hexify-string fen)))
    (lichess-http-json
     (format "/api/cloud-eval?fen=%s" encoded-fen) callback nil
     t))) ;; anonymous = true (cloud eval is public/anon)

;;; Board API (Moves/Game)
(defun lichess-api-board-move (game-id move callback)
  "Make a MOVE (UCI) in GAME-ID."
  (lichess-http-request
   (format "/api/board/game/%s/move/%s" game-id move) callback
   :method "POST"))

(defun lichess-api-board-resign (game-id callback)
  "Resign GAME-ID.  CALLBACK: (STATUS . DATA)."
  (lichess-http-request
   (format "/api/board/game/%s/resign" game-id)
   callback
   :method "POST"))

(defun lichess-api-board-draw (game-id answer callback)
  "Offer or accept draw in GAME-ID.
ANSWER is `yes' or `no' (to decline).
CALLBACK: (STATUS . DATA)."
  (lichess-http-request
   (format "/api/board/game/%s/draw/%s"
           game-id
           (symbol-name answer))
   callback
   :method "POST"))

(defun lichess-api-stream-game-url (game-id)
  "Return NDJSON stream URL for spectator GAME-ID."
  (format "/api/stream/game/%s" game-id))

(defun lichess-api-stream-game-board-url (game-id)
  "Return NDJSON stream URL for playing GAME-ID (Board API)."
  (format "/api/board/game/stream/%s" game-id))

(provide 'lichess-api)
;;; lichess-api.el ends here
