;;; lichess-challenge.el --- Challenge friends on Lichess -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026  Alexandr Timchenko
;; URL: https://github.com/tmythicator/Lichess.el
;; Version: 0.8
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; Logic for challenging friends/users and starting games.
;; API: POST /api/challenge/{username}
;;
;;; Code:

(require 'lichess-http)
(require 'lichess-api)
(require 'lichess-game)
(require 'lichess-util)
(require 'lichess-challenge-list)

(defcustom lichess-challenge-default-clock-limit 300
  "Default clock limit in seconds (5 minutes)."
  :type 'integer
  :group 'lichess)

(defcustom lichess-challenge-default-clock-increment 3
  "Default clock increment in seconds."
  :type 'integer
  :group 'lichess)

(defvar lichess-challenge--event-stream nil
  "Internal handle for the event stream listener.")

(defun lichess-challenge--get-friends (callback)
  "Fetch following list and call CALLBACK with an alist of (NAME . ID)."
  (lichess-api-get-following
   (lambda (res)
     (let ((status (car res))
           (data (cdr res)))
       (if (= status 200)
           (let* ((lines (split-string (string-trim data) "\n" t))
                  (friends
                   (mapcar
                    (lambda (line)
                      (condition-case nil
                          (let* ((obj (json-read-from-string line))
                                 (id (lichess-util--aget obj 'id))
                                 (name
                                  (lichess-util--aget obj 'name)))
                            (if (and id name)
                                (cons name id)
                              (cons (or name id line) (or id line))))
                        (error
                         (cons line line))))
                    lines)))
             (funcall callback :ok friends))
         (if (= status 403)
             (funcall callback :missing-scope nil)
           (message "Error fetching friends: %d" status)
           (funcall callback :error nil)))))))

;;;###autoload
(defun lichess-challenge-user ()
  "Prompt for a user to challenge and start the game when accepted."
  (interactive)
  (lichess-challenge--get-friends
   (lambda (res-type friends)
     (let*
         ((display-names (mapcar #'car friends))
          (prompt
           (cond
            ((eq res-type :missing-scope)
             "Challenge User (missing 'follow:read' scope - type username): ")
            ((null friends)
             "Challenge User (no friends found - type username): ")
            (t
             "Challenge User: ")))
          (input (completing-read prompt display-names nil nil))
          (username-id (or (cdr (assoc input friends)) input))
          (rated (y-or-n-p "Rated? "))
          (variant
           (completing-read "Variant: " lichess-core-variants
                            nil t "standard"))
          (color
           (completing-read "Your Color: " '("white" "black" "random")
                            nil t "random"))
          (limit
           (read-number "Clock limit (seconds): "
                        lichess-challenge-default-clock-limit))
          (increment
           (read-number "Clock increment (seconds): "
                        lichess-challenge-default-clock-increment)))
       (if (string-empty-p input)
           (message "No username provided, challenge cancelled.")
         (lichess-challenge--send
          username-id rated color limit increment variant))))))

(defun lichess-challenge--send
    (username rated color limit increment variant)
  "Send the challenge request to USERNAME.
RATED, COLOR, LIMIT, INCREMENT, and VARIANT specify the game parameters."
  (message "Challenging %s (%s)..." username variant)
  (lichess-api-challenge-user
   username rated (intern color) limit increment variant
   (lambda (res)
     (let ((status (car res))
           (json (cdr res)))
       (if (memq status '(200 201))
           (progn
             (message
              "Challenge (%s) sent to %s! Waiting for acceptance..."
              variant username)
             (lichess-challenge--listen-for-start)
             (lichess-challenge-list))
         (message "Error challenging %s: %d %s"
                  username
                  status
                  (or (lichess-util--aget json 'error) "")))))))

(defun lichess-challenge--listen-for-start ()
  "Start listening to the event stream for game start."
  (unless lichess-challenge--event-stream
    (setq lichess-challenge--event-stream
          (lichess-http-ndjson-open
           (lichess-api-stream-event-url)
           :on-event #'lichess-challenge--handle-event
           :on-close
           (lambda (_p _m)
             (setq lichess-challenge--event-stream nil))))))

(defun lichess-challenge--handle-event (obj)
  "Handle an event OBJ from the Lichess event stream."
  (let ((type (lichess-util--aget obj 'type)))
    (cond
     ((string= type "gameStart")
      (let* ((game (lichess-util--aget obj 'game))
             (id (lichess-util--aget game 'id)))
        (when id
          (message "Game started! ID: %s" id)
          ;; Close event stream if we are just waiting for this one game
          (when lichess-challenge--event-stream
            (lichess-http-ndjson-close
             lichess-challenge--event-stream)
            (setq lichess-challenge--event-stream nil))
          (lichess-game-play id))))

     ((string= type "challengeCanceled")
      (let* ((ch (lichess-util--aget obj 'challenge))
             (id (lichess-util--aget ch 'id)))
        (message "Challenge %s canceled." id)
        (when lichess-challenge--event-stream
          (lichess-http-ndjson-close lichess-challenge--event-stream)
          (setq lichess-challenge--event-stream nil))))

     ((string= type "challengeDeclined")
      (let* ((ch (lichess-util--aget obj 'challenge))
             (id (lichess-util--aget ch 'id))
             (dest-user
              (lichess-util--aget
               (lichess-util--aget ch 'destUser) 'name)))
        (message "Challenge %s declined by %s."
                 id
                 (or dest-user "opponent"))
        (when lichess-challenge--event-stream
          (lichess-http-ndjson-close lichess-challenge--event-stream)
          (setq lichess-challenge--event-stream nil)))))))

(provide 'lichess-challenge)
;;; lichess-challenge.el ends here
