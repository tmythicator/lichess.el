;;; lichess-util.el --- Util functions for Lichess -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025  Alexandr Timchenko
;; SPDX-License-Identifier: GPL-3.0-or-later
;; See LICENSE for details.
;;
;;; Commentary:
;;
;;  Util functions for lichess.el
;;
;;; Code:

(defun lichess-util--aget (obj key)
  "Safe get from OBJ (alist or hash-table). KEY may be symbol or string."
  (cond
   ((hash-table-p obj)
    (or (gethash key obj)
        (and (symbolp key) (gethash (symbol-name key) obj))
        (and (stringp key) (gethash (intern key) obj))))
   ((consp obj)
    (or (alist-get key obj nil nil)
        (and (symbolp key) (alist-get (symbol-name key) obj nil nil #'string=))
        (and (stringp key) (alist-get (intern key) obj nil nil))))
   (t nil)))

(defun lichess-util--fmt-player (name title rating)
  "Return compact 'TITLE NAME (RATING)' or 'Anonymous'."
  (if name
      (string-trim
       (format "%s%s%s"
               (or title "")
               (if title " " "")
               (if rating (format "%s (%s)" name rating) name)))
    "Anonymous"))

(defun lichess-util--insert-propertized-line (text id)
  "Insert TEXT, ensure trailing \\n, add props for the whole line, return BOL marker."
  (let ((beg (point)))
    (insert text)
    (unless (and (> (length text) 0)
                 (eq (aref text (1- (length text))) ?\n))
      (insert "\n"))
    (let ((bol beg)
          (eol (line-end-position 0)))
      (add-text-properties bol (min (point-max) (1+ eol))
                           (list 'lichess-game-id id
                                 'mouse-face 'highlight
                                 'help-echo "RET: open in browser"))
      (copy-marker bol nil))))

;; Formatted Lichess TV string
(defun lichess-util--game->vs (game)
  "Build \"White vs Black\" string from /api/game JSON alist."
  (let* ((players (lichess-util--aget game 'players))
         (w (lichess-util--aget players 'white))
         (b (lichess-util--aget players 'black))
         (wu (lichess-util--aget w 'user))
         (bu (lichess-util--aget b 'user))
         (w-name (or (lichess-util--aget wu 'name)
                     (lichess-util--aget w 'userId)))
         (b-name (or (lichess-util--aget bu 'name)
                     (lichess-util--aget b 'userId)))
         (w-title (lichess-util--aget wu 'title))
         (b-title (lichess-util--aget bu 'title))
         (w-rating (lichess-util--aget w 'rating))
         (b-rating (lichess-util--aget b 'rating)))
    (format "%s  vs  %s"
            (lichess-util--fmt-player w-name w-title w-rating)
            (lichess-util--fmt-player b-name b-title b-rating))))

(provide 'lichess-util)
;;; lichess-util.el ends here