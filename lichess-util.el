;;; lichess-util.el --- Util functions for Lichess -*- lexical-binding: t; -*-
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

(defun lichess-util-fmt-user-obj (user-obj)
  "Extract a readable name from a Lichess player/user USER-OBJ.
Handles nested `user' keys, titles, ratings, and AI levels."
  (let* ((user (or (lichess-util--aget user-obj 'user) user-obj))
         (name
          (or (lichess-util--aget user 'name)
              (lichess-util--aget user 'username)
              (lichess-util--aget user 'id)
              (lichess-util--aget user 'userId)))
         (title (lichess-util--aget user 'title))
         (rating (lichess-util--aget user-obj 'rating))
         (ai-level
          (or (lichess-util--aget user-obj 'aiLevel)
              (lichess-util--aget user-obj 'ai))))
    (cond
     (name
      (string-trim
       (format "%s%s%s"
               (or title "")
               (if title
                   " "
                 "")
               (if rating
                   (format "%s (%s)" name rating)
                 name))))
     (ai-level
      (format "Stockfish level %d" ai-level))
     (t
      "Anonymous"))))

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
         (b (lichess-util--aget players 'black)))
    (format "%s  vs  %s"
            (lichess-util-fmt-user-obj w)
            (lichess-util-fmt-user-obj b))))

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
                        (if (eq (car res) 200)
                            (when (functionp callback)
                              (let* ((data (cdr res))
                                     (pvs
                                      (lichess-util--aget data 'pvs))
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
                                        :unavailable))))
                                (funcall callback eval-str)))
                          ;; Non-200 response (e.g. 404)
                          (when (functionp callback)
                            (funcall callback :unavailable)))))))
                 fen callback)))

(provide 'lichess-util)
;;; lichess-util.el ends here
