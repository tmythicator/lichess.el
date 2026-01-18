;;; lichess-util.el --- Util functions for Lichess -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026  Alexandr Timchenko
;; URL: https://github.com/tmythicator/Lichess.el
;; Version: 0.4
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; See LICENSE for details.
;;
;;; Commentary:
;;
;;  Util functions for lichess.el
;;
;;; Code:

(require 'lichess-http)

(defcustom lichess-util-eval-delay 1.0
  "Minimum delay in seconds between cloud evaluation requests."
  :type 'number
  :group 'lichess)

(defvar lichess-util--next-eval-time 0.0
  "The earliest time the next cloud-eval request can be sent.")

(defun lichess-util--aget (obj key)
  "Safe get from OBJ (alist or hash-table).  KEY may be symbol or string."
  (cond
   ((hash-table-p obj)
    (let ((str
           (if (symbolp key)
               (symbol-name key)
             key))
          (sym
           (if (stringp key)
               (intern key)
             key)))
      (or (gethash sym obj) (gethash str obj))))
   ((consp obj)
    (let ((str
           (if (symbolp key)
               (symbol-name key)
             key))
          (sym
           (if (stringp key)
               (intern key)
             key)))
      (or (alist-get sym obj nil nil #'eq)
          (alist-get str obj nil nil #'string=))))
   (t
    nil)))

(defun lichess-util--fmt-player (name title rating)
  "Return compact TITLE NAME (RATING) or `Anonymous'."
  (if name
      (string-trim
       (format "%s%s%s"
               (or title "")
               (if title
                   " "
                 "")
               (if rating
                   (format "%s (%s)" name rating)
                 name)))
    "Anonymous"))

(defun lichess-util--insert-propertized-line (text id)
  "Insert TEXT, ensure trailing \\n, add props for the whole line.
ID is the game ID.  Return BOL marker."
  (let ((beg (point)))
    (insert text)
    (unless (and (> (length text) 0)
                 (eq (aref text (1- (length text))) ?\n))
      (insert "\n"))
    (let ((bol beg)
          (eol (line-end-position 0)))
      (add-text-properties
       bol (min (point-max) (1+ eol))
       (list
        'lichess-game-id
        id
        'mouse-face
        'highlight
        'help-echo
        "RET: open in browser"))
      (copy-marker bol nil))))

;; Formatted Lichess TV string
(defun lichess-util--game->vs (game)
  "Build \"White vs Black\" string from /api/game JSON GAME."
  (let* ((players (lichess-util--aget game 'players))
         (w (lichess-util--aget players 'white))
         (b (lichess-util--aget players 'black))
         (wu (lichess-util--aget w 'user))
         (bu (lichess-util--aget b 'user))
         (w-name
          (or (lichess-util--aget wu 'name)
              (lichess-util--aget w 'userId)))
         (b-name
          (or (lichess-util--aget bu 'name)
              (lichess-util--aget b 'userId)))
         (w-title (lichess-util--aget wu 'title))
         (b-title (lichess-util--aget bu 'title))
         (w-rating (lichess-util--aget w 'rating))
         (b-rating (lichess-util--aget b 'rating)))
    (format "%s  vs  %s"
            (lichess-util--fmt-player w-name w-title w-rating)
            (lichess-util--fmt-player b-name b-title b-rating))))

(defun lichess-util-fetch-evaluation (fen callback)
  "Fetch cloud evaluation for a FEN with rate-limiting.
Passes the result string to CALLBACK."
  (let* ((current-time (float-time))
         (scheduled-time
          (max current-time lichess-util--next-eval-time)))
    (setq lichess-util--next-eval-time
          (+ scheduled-time lichess-util-eval-delay))
    (run-at-time (- scheduled-time current-time) nil
                 (lambda (fen callback)
                   (let ((url
                          (format "/api/cloud-eval?fen=%s"
                                  (url-hexify-string fen))))
                     (lichess-http-json
                      url
                      (lambda (res)
                        (message "FEN: %s.\n eval RES: %s" fen res)
                        (when (and (eq (car res) 200)
                                   (functionp callback))
                          (let* ((data (cdr res))
                                 (pvs (lichess-util--aget data 'pvs))
                                 (best-pv (and pvs (car pvs)))
                                 (cp
                                  (and best-pv
                                       (lichess-util--aget
                                        best-pv 'cp)))
                                 (mate
                                  (and best-pv
                                       (lichess-util--aget
                                        best-pv 'mate)))
                                 (eval-str
                                  (cond
                                   (mate
                                    (format "M%d" mate))
                                   (cp
                                    (format "%+.2f"
                                            (/ (float cp) 100.0)))
                                   (t
                                    nil))))
                            (when eval-str
                              (message "eval: %s" eval-str)
                              (funcall callback eval-str))))))))
                 fen callback)))

(provide 'lichess-util)
;;; lichess-util.el ends here
