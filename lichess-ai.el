;;; lichess-ai.el --- Play against Lichess AI -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026  Alexandr Timchenko
;; URL: https://github.com/tmythicator/Lichess.el
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; Logic for challenging Lichess AI (Stockfish) and starting games.
;; API: POST /api/challenge/ai
;;
;;; Code:

(require 'lichess-http)
(require 'lichess-api)
(require 'lichess-game)
(require 'lichess-util)
(require 'url-util)

(defcustom lichess-ai-default-level 1
  "Default AI level (1-8)."
  :type 'integer
  :group 'lichess)

(defcustom lichess-ai-default-clock-limit 5
  "Default clock limit in minutes."
  :type 'integer
  :group 'lichess)

(defcustom lichess-ai-default-clock-increment 10
  "Default clock increment in seconds."
  :type 'integer
  :group 'lichess)

;;;###autoload
(defun lichess-ai-challenge ()
  "Prompt for AI game parameters and start a game."
  (interactive)
  (let* ((level
          (read-number "AI Level (1-8): " lichess-ai-default-level))
         (color
          (completing-read "Your Color: " '("white" "black" "random")
                           nil t "white"))
         (limit-min
          (read-number "Clock limit (minutes): "
                       lichess-ai-default-clock-limit))
         (increment
          (read-number "Clock increment (seconds): "
                       lichess-ai-default-clock-increment)))
    (lichess-ai--start-game level color (* limit-min 60) increment)))

(defun lichess-ai--start-game (level color limit increment)
  "Send the POST request to Lichess to start a game.
LEVEL is the AI strength.  COLOR is the player color.
LIMIT and INCREMENT define the time control."
  (message "Challenging Lichess AI level %d..." level)
  (lichess-api-challenge-ai
   level (intern color) limit increment nil
   (lambda (res)
     (if (lichess-http-result-success res)
         (let* ((json (lichess-http-result-data res))
                (id (lichess-util--aget json 'id)))
           (if id
               (progn
                 (message "Game started! ID: %s" id)
                 (lichess-game-play id))
             (message "Error: No game ID returned from Lichess.")))
       (let* ((err (lichess-http-result-error res))
              (status (car err))
              (json (cdr err)))
         (message "Lichess AI error: %d %s"
                  status
                  (or (lichess-util--aget json 'error) "")))))
   nil))


(provide 'lichess-ai)
;;; lichess-ai.el ends here
