;;; lichess-seek.el --- Seek games on Lichess -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026  Alexandr Timchenko
;; URL: https://github.com/tmythicator/Lichess.el
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; Matchmaking / seek games on Lichess.
;; API: POST /api/board/seek
;;
;;; Code:

(require 'lichess-core)
(require 'lichess-http)
(require 'lichess-api)
(require 'lichess-game)
(require 'lichess-util)
(require 'lichess-challenge)

(defgroup lichess-seek nil
  "Seeking games on Lichess."
  :group 'lichess)

(defcustom lichess-seek-default-time 5
  "Default real-time seek initial clock time in minutes."
  :type 'number
  :group 'lichess-seek)

(defcustom lichess-seek-default-increment 10
  "Default real-time seek clock increment in seconds."
  :type 'integer
  :group 'lichess-seek)

(defvar lichess-seek--active-stream nil
  "Handle for the active real-time seek stream process.")

(defvar lichess-seek--cancelling nil
  "Non-nil if the current seek is being manually cancelled.")

;;;###autoload
(defun lichess-seek-cancel ()
  "Cancel the active real-time seek, if any."
  (interactive)
  (if lichess-seek--active-stream
      (progn
        (setq lichess-seek--cancelling t)
        (lichess-http-stream-close lichess-seek--active-stream)
        (setq lichess-seek--active-stream nil)
        (setq lichess-seek--cancelling nil)
        (message "Seek cancelled."))
    (message "No active seek to cancel.")))

;;;###autoload
(defun lichess-seek-game ()
  "Interactively seek a game with another player.
Choose between Real-time or Correspondence, rated or casual, chess variant,
and preferred color."
  (interactive)
  (let* ((seek-type
          (completing-read
           "Seek Type: " '("Real-time" "Correspondence")
           nil t "Real-time"))
         (rated (y-or-n-p "Rated? "))
         (color
          (completing-read "Your Color: " '("random" "white" "black")
                           nil t "random"))
         (variant
          (completing-read "Variant: " lichess-core-variants
                           nil
                           t
                           "standard"))
         (rating-range
          (read-string "Rating Range (optional, e.g., 1500-1800): ")))
    (if (string= seek-type "Real-time")
        (let ((time
               (read-number "Time limit (minutes): "
                            lichess-seek-default-time))
              (increment
               (read-number "Increment (seconds): "
                            lichess-seek-default-increment)))
          (lichess-seek--real-time
           time increment rated color variant rating-range))
      (let ((days
             (completing-read
              "Days per turn: " '("1" "2" "3" "5" "7" "10" "14")
              nil t "1")))
        (lichess-seek--correspondence
         (string-to-number days) rated color variant rating-range)))))

(defun lichess-seek--real-time
    (time increment rated color variant rating-range)
  "Create a real-time seek for TIME (min) and INCREMENT (sec).
RATED is boolean, COLOR is the preferred color, VARIANT is the game variant,
and RATING-RANGE is optional opponent rating filter."
  (when lichess-seek--active-stream
    (when
        (y-or-n-p
         "An active seek is already running.  Cancel it and start new one? ")
      (lichess-seek-cancel)))
  (unless lichess-seek--active-stream
    ;; Make sure the event listener is active so we get the gameStart event.
    (lichess-challenge--listen-for-start)
    (message "Opening seek connection to Lichess...")
    (setq
     lichess-seek--active-stream
     (lichess-api-board-seek-stream
      time increment
      (if rated
          "true"
        "false")
      variant
      (unless (string= color "random")
        color)
      (unless (string-empty-p rating-range)
        rating-range)
      :on-open
      (lambda (_proc _buf)
        (message
         "Seeking game (%s %s+%s, rated: %s)... Press M-x lichess-seek-cancel to cancel."
         variant
         (number-to-string time)
         (number-to-string increment)
         (if rated
             "yes"
           "no")))
      :on-event
      (lambda (obj)
        (let ((err (lichess-util--aget obj 'error)))
          (when err
            (message "Seek error: %s" err)
            ;; Close the failed stream
            (setq lichess-seek--cancelling t)
            (lichess-http-stream-close lichess-seek--active-stream)
            (setq lichess-seek--active-stream nil)
            (setq lichess-seek--cancelling nil))))
      :on-close
      (lambda (_proc _msg)
        (unless lichess-seek--cancelling
          (message
           "Seek connection closed by Lichess. (Seek accepted or expired)"))
        (setq lichess-seek--active-stream nil))))))

(defun lichess-seek--correspondence
    (days rated color variant rating-range)
  "Create a correspondence seek for DAYS per turn.
RATED is boolean, COLOR is the preferred color, VARIANT is the game variant,
and RATING-RANGE is optional opponent rating filter."
  (message "Creating correspondence seek on Lichess...")
  (lichess-api-board-seek-correspondence
   days rated variant color
   (if (string-empty-p rating-range)
       nil
     rating-range)
   (lambda (res)
     (if (lichess-http-result-success res)
         (let* ((data (lichess-http-result-data res))
                (id (lichess-util--aget data 'id)))
           (message
            "Correspondence seek created successfully! ID: %s. Wait for a player to join."
            id))
       (let* ((err (lichess-http-result-error res))
              (status (car err))
              (json (cdr err)))
         (message "Error creating seek: %d %s"
                  status
                  (or (lichess-util--aget json 'error) "")))))))

(provide 'lichess-seek)
;;; lichess-seek.el ends here
