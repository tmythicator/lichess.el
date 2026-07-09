;;; lichess-api.el --- Lichess API Endpoints -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026  Alexandr Timchenko
;; URL: https://github.com/tmythicator/Lichess.el
;; Version: 0.9
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

(defvar lichess-api-request-function #'lichess-http-request
  "Function used to perform HTTP requests.
It must accept: (ENDPOINT CALLBACK &rest PLIST).")

(defun lichess-api--call-json
    (endpoint callback &optional headers anonymous)
  "GET JSON from ENDPOINT and call CALLBACK.
Optional HEADERS is an alist of headers.
If ANONYMOUS is non-nil, the request does not include authorization."
  (funcall lichess-api-request-function
           endpoint
           callback
           :method "GET"
           :accept "application/json"
           :headers headers
           :parse 'json
           :anonymous anonymous))

(defun lichess-api--call-post
    (endpoint params callback &optional parse-type)
  "POST to ENDPOINT with PARAMS and call CALLBACK.
PARAMS is an alist of key-value parameters.
PARSE-TYPE controls response parsing: `json' (default) or `raw'."
  (funcall lichess-api-request-function
           endpoint callback
           :method "POST"
           :data (and params (url-build-query-string params))
           :headers
           (and params
                '(("Content-Type"
                   .
                   "application/x-www-form-urlencoded")))
           :parse (or parse-type 'json)))

;;; TV
(defun lichess-api-get-tv-channels (callback)
  "Fetch TV channels.  CALLBACK received (STATUS . DATA)."
  (lichess-api--call-json "/api/tv/channels" callback))

;;; Broadcasts
(defun lichess-api-get-broadcasts (callback &optional nb)
  "Fetch top broadcasts.  CALLBACK is called with (STATUS . DATA).
NB is count (default 20)."
  (lichess-api--call-json
   (format "/api/broadcast/top?nb=%d" (or nb 20)) callback))

(defun lichess-api-get-broadcast-round (url callback)
  "Fetch broadcast round data for URL.
Convert URL to API path:
`https://lichess.org/{fullbroadcastlink}` into
`/api/{fullbroadcastlink}`.
CALLBACK is called with (STATUS . DATA)."
  (let ((path (replace-regexp-in-string "^.*lichess.org" "/api" url)))
    (lichess-api--call-json path callback nil t)))

(defun lichess-api-get-game (game-id callback)
  "Fetch game data for GAME-ID.  CALLBACK: (STATUS . DATA)."
  (lichess-api--call-json (format "/api/game/%s" game-id) callback))

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
  (let ((params
         `(("level" ,(number-to-string level))
           ("color" ,(symbol-name color))
           ("clock.limit" ,(number-to-string limit))
           ("clock.increment" ,(number-to-string increment)))))
    (when fen
      (push `("fen" ,fen) params))
    (lichess-api--call-post "/api/challenge/ai" params callback
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
CALLBACK: (STATUS . DATA)."
  (let ((params
         `(("rated" ,(if rated
                 "true"
               "false"))
           ("color" ,(symbol-name color))
           ("clock.limit" ,(number-to-string limit))
           ("clock.increment" ,(number-to-string increment))
           ("variant" ,variant))))
    (lichess-api--call-post
     (format "/api/challenge/%s" username) params callback)))

(defun lichess-api-get-challenges (callback)
  "Fetch current challenges (incoming and outgoing).
CALLBACK: (STATUS . DATA)."
  (lichess-api--call-json "/api/challenge" callback))

(defun lichess-api-cancel-challenge (id callback)
  "Cancel challenge with ID.
CALLBACK: (STATUS . DATA)."
  (lichess-api--call-post
   (format "/api/challenge/%s/cancel" id) nil callback))

(defun lichess-api-accept-challenge (id callback)
  "Accept challenge with ID.
CALLBACK: (STATUS . DATA)."
  (lichess-api--call-post
   (format "/api/challenge/%s/accept" id) nil callback))

(defun lichess-api-get-following (callback)
  "Fetch the list of users followed by current user.
CALLBACK: (STATUS . DATA)."
  (funcall lichess-api-request-function
           "/api/rel/following"
           callback
           :accept "application/x-ndjson"
           :parse 'raw))

;;; Cloud Eval
(defun lichess-api-cloud-eval (fen callback)
  "Fetch cloud evaluation for FEN.
CALLBACK receives evaluation string or :unavailable."
  (let ((encoded-fen (url-hexify-string fen)))
    (lichess-api--call-json (format "/api/cloud-eval?fen=%s"
                                    encoded-fen)
                            callback
                            nil t)))

;;; Board API (Moves/Game)
(defun lichess-api-board-move (game-id move callback)
  "Make a MOVE (UCI) in GAME-ID."
  (lichess-api--call-post
   (format "/api/board/game/%s/move/%s" game-id move) nil callback))

(defun lichess-api-board-resign (game-id callback)
  "Resign GAME-ID.  CALLBACK: (STATUS . DATA)."
  (lichess-api--call-post
   (format "/api/board/game/%s/resign" game-id) nil callback))

(defun lichess-api-board-draw (game-id answer callback)
  "Offer or accept draw in GAME-ID.
ANSWER is `yes' or `no' (to decline).
CALLBACK: (STATUS . DATA)."
  (lichess-api--call-post
   (format "/api/board/game/%s/draw/%s"
           game-id
           (symbol-name answer))
   nil callback))

(defun lichess-api-stream-game-url (game-id)
  "Return NDJSON stream URL for spectator GAME-ID."
  (format "/api/stream/game/%s" game-id))

(defun lichess-api-stream-game-board-url (game-id)
  "Return NDJSON stream URL for playing GAME-ID (Board API)."
  (format "/api/board/game/stream/%s" game-id))

(defun lichess-api-stream-event-url ()
  "Return NDJSON stream URL for incoming events."
  "/api/stream/event")

(provide 'lichess-api)
;;; lichess-api.el ends here
