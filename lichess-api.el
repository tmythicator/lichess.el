;;; lichess-api.el --- Lichess API Endpoints -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026  Alexandr Timchenko
;; URL: https://github.com/tmythicator/Lichess.el
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Centralized API definitions for Lichess.el.
;; All external API calls should be routed through this module.

;;; Code:

(require 'lichess-http)

;;;; API Endpoint Definitions

(lichess-http-defendpoint
 lichess-api-get-tv-channels
 "/api/tv/channels"
 "Fetch TV channels. CALLBACK receives `lichess-http-result`.")

(lichess-http-defendpoint
 lichess-api-get-broadcasts
 "/api/broadcast/top"
 "Fetch top broadcasts. CALLBACK receives `lichess-http-result`."
 :query-params (nb))

(defun lichess-api-get-broadcast-round (url callback)
  "Fetch broadcast round data for URL.
Convert URL to API path:
`https://lichess.org/{fullbroadcastlink}` into `/api/{fullbroadcastlink}`.
CALLBACK is called with a `lichess-http-result`."
  (let ((path (replace-regexp-in-string "^.*lichess.org" "/api" url)))
    (lichess-http--call-get path callback nil t)))

(lichess-http-defendpoint
 lichess-api-get-game
 "/api/game/:game-id"
 "Fetch game data for GAME-ID. CALLBACK: `lichess-http-result`."
 :path-params (game-id))

(defun lichess-api-challenge-ai
    (level color limit increment fen callback text-mode)
  "Challenge AI.
LEVEL: 1-8.
COLOR: `white', `black', or `random'.
LIMIT: Clock limit in seconds.
INCREMENT: Clock increment in seconds.
FEN: Optional starting position.
CALLBACK: `lichess-http-result`.
TEXT-MODE: If non-nil, parse response as `raw' text."
  (let ((params
         `(("level" ,(number-to-string level))
           ("color" ,(symbol-name color))
           ("clock.limit" ,(number-to-string limit))
           ("clock.increment" ,(number-to-string increment)))))
    (when fen
      (push `("fen" ,fen) params))
    (lichess-http--call-post
     "/api/challenge/ai" params callback
     (if text-mode
         'raw
       'json))))

(defun lichess-api-challenge-user
    (username rated color limit increment variant callback)
  "Challenge USERNAME.
RATED: Boolean.
COLOR: `white', `black', or `random'.
LIMIT: Clock limit in seconds.
INCREMENT: Clock increment in seconds.
VARIANT: e.g., \"standard\".
CALLBACK: `lichess-http-result`."
  (let ((params
         `(("rated" ,(if rated
                 "true"
               "false"))
           ("color" ,(symbol-name color))
           ("clock.limit" ,(number-to-string limit))
           ("clock.increment" ,(number-to-string increment))
           ("variant" ,variant))))
    (lichess-http--call-post
     (format "/api/challenge/%s" username) params callback)))

(lichess-http-defendpoint
 lichess-api-get-challenges
 "/api/challenge"
 "Fetch current challenges. CALLBACK: `lichess-http-result`.")

(lichess-http-defendpoint
 lichess-api-cancel-challenge
 "/api/challenge/:id/cancel"
 "Cancel challenge with ID. CALLBACK: `lichess-http-result`."
 :method POST
 :path-params (id))

(lichess-http-defendpoint
 lichess-api-accept-challenge
 "/api/challenge/:id/accept"
 "Accept challenge with ID. CALLBACK: `lichess-http-result`."
 :method POST
 :path-params (id))

(lichess-http-defendpoint
 lichess-api-get-following "/api/rel/following"
 "Fetch the list of users followed by current user.
CALLBACK: `lichess-http-result`."
 :parse-type raw
 :accept-header "application/x-ndjson")

(defun lichess-api-cloud-eval (fen callback)
  "Fetch cloud evaluation for FEN.
CALLBACK receives `lichess-http-result`."
  (let ((encoded-fen (url-hexify-string fen)))
    (lichess-http--call-get
     (format "/api/cloud-eval?fen=%s" encoded-fen) callback nil t)))

(lichess-http-defendpoint
 lichess-api-board-move
 "/api/board/game/:game-id/move/:move"
 "Make a MOVE (UCI) in GAME-ID. CALLBACK: `lichess-http-result`."
 :method POST
 :path-params (game-id move))

(lichess-http-defendpoint
 lichess-api-board-resign
 "/api/board/game/:game-id/resign"
 "Resign GAME-ID. CALLBACK: `lichess-http-result`."
 :method POST
 :path-params (game-id))

(lichess-http-defendpoint
 lichess-api-board-draw "/api/board/game/:game-id/draw/:answer"
 "Offer or accept draw in GAME-ID.
ANSWER is `yes' or `no'.
CALLBACK: `lichess-http-result`."
 :method POST
 :path-params (game-id answer))

(lichess-http-defstream
 lichess-api-stream-game
 "/api/stream/game/:game-id"
 "Open spectator stream for GAME-ID."
 :path-params (game-id))

(lichess-http-defstream
 lichess-api-stream-game-board
 "/api/board/game/stream/:game-id"
 "Open playing stream for GAME-ID."
 :path-params (game-id))

(lichess-http-defstream
 lichess-api-stream-event "/api/stream/event" "Open event stream.")

(lichess-http-defstream
 lichess-api-board-seek-stream
 "/api/board/seek"
 "Open a real-time seek stream."
 :method POST
 :post-params
 (time increment rated variant color ratingRange))

(lichess-http-defendpoint
 lichess-api-board-seek-correspondence
 "/api/board/seek"
 "Create a correspondence seek. CALLBACK receives `lichess-http-result`."
 :method POST
 :post-params (days rated variant color ratingRange))

(provide 'lichess-api)
;;; lichess-api.el ends here
