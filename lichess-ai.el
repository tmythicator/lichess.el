;;; lichess-ai.el --- Play against Lichess AI -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025  Alexandr Timchenko
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; Logic for challenging Lichess AI (Stockfish) and starting games.
;; API: POST /api/challenge/ai
;;
;;; Code:

(require 'lichess-http)
(require 'lichess-game)
(require 'lichess-util)
(require 'url-util)

(defcustom lichess-ai-default-level 1
  "Default AI level (1-8)."
  :type 'integer :group 'lichess)

(defcustom lichess-ai-default-clock-limit 300
  "Default clock limit in seconds (5 minutes)."
  :type 'integer :group 'lichess)

(defcustom lichess-ai-default-clock-increment 3
  "Default clock increment in seconds."
  :type 'integer :group 'lichess)

;;;###autoload
(defun lichess-ai-challenge ()
  "Prompt for AI game parameters and start a game."
  (interactive)
  (let* ((level (read-number "AI Level (1-8): " lichess-ai-default-level))
         (color (completing-read "Your Color: " '("white" "black" "random") nil t "white"))
         (limit (read-number "Clock limit (seconds): " lichess-ai-default-clock-limit))
         (increment (read-number "Clock increment (seconds): " lichess-ai-default-clock-increment)))
    (lichess-ai--start-game level color limit increment)))

(defun lichess-ai--start-game (level color limit increment)
  "Actually send the POST request to Lichess."
  (message "Challenging Lichess AI level %d..." level)
  (let* ((data (format "level=%d&color=%s&clock.limit=%d&clock.increment=%d"
                       level color limit increment)))
    (lichess-http-request
     "/api/challenge/ai"
     (lambda (res)
       (let ((status (car res))
             (json (cdr res)))
         (if (memq status '(200 201))
             (let ((id (lichess-util--aget json 'id)))
               (if id
                   (progn
                     (message "Game started! ID: %s" id)
                     (lichess-game-play id))
                 (message "Error: No game ID returned from Lichess.")))
           (message "Lichess AI error: %d %s" status (or (lichess-util--aget json 'error) "")))))
     :method "POST"
     :data data
     :headers '(("Content-Type" . "application/x-www-form-urlencoded")))))


(provide 'lichess-ai)
;;; lichess-ai.el ends here
