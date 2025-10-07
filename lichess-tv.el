;;; lichess-tv.el --- Lichess TV integration for Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025  Alexandr Timchenko
;; SPDX-License-Identifier: GPL-3.0-or-later
;; See LICENSE for details.
;;
;;; Commentary:
;;
;; Live Lichess TV client:
;; - `M-x lichess-tv` to list all TV channels
;; - `M-x lichess-tv-debug` to inspect raw /api/tv/channels JSON
;; - `g` refreshes the list
;; - `RET` (planned) will open a game buffer
;;
;;; Code:

(require 'cl-lib)
(require 'lichess-core)

(defcustom lichess-tv-refresh-seconds 5
  "Auto-refresh period for best view."
  :type 'integer :group 'lichess)

(defvar lichess-tv--buf "*Lichess TV*")
(defvar lichess-tv-debug--buf "*(Debug) Lichess TV*")

;;;###autoload
(defun lichess-tv ()
  "Show current Lichess TV channels; lines are clickable (RET)."
  (interactive)
  (let ((buf (get-buffer-create lichess-tv--buf)))
    (with-current-buffer buf
      (lichess-core-mode)
      (local-set-key (kbd "g") #'lichess-tv)) ;; refresh
    (lichess-core-with-buf buf
                           (lambda ()
                             (erase-buffer)
                             (insert "Fetching Lichess TV channels…\n")))
    (pop-to-buffer buf))
  (lichess-core-fetch-json "https://lichess.org/api/tv/channels"
                           #'lichess-tv--handle-channels))

(defun lichess-tv--handle-channels (res)
  "Process /api/tv/channels response."
  (if (/= (car res) 200)
      (lichess-core-with-buf (get-buffer lichess-tv--buf)
                             (lambda ()
                               (erase-buffer)
                               (insert (format "HTTP %s from /api/tv/channels\n" (car res)))))
    (mapc #'lichess-tv--insert-channel (cdr res))))

(defun lichess-tv--insert-channel (pair)
  "Insert one channel entry PAIR = (CHAN . GAME)."
  (pcase-let* ((`(,chan . ,g) pair)
               (chan-name (symbol-name chan))
               (id        (or (alist-get 'gameId g)
                              (alist-get 'id g)))
               (user      (alist-get 'user g))
               (name      (alist-get 'name user))
               (title     (alist-get 'title user))
               (rating    (alist-get 'rating g))
               (color     (alist-get 'color g))
               (inline    (lichess-tv--fmt-inline name title rating)))
    (lichess-core-with-buf (get-buffer lichess-tv--buf)
                           (lambda ()
                             (goto-char (point-max))
                             (let* ((pos (point))
                                    (marker (copy-marker pos t)))
                               (insert (format "%-12s  %-28s (%s)  id:%s\n"
                                               chan-name inline color id))
                               (add-text-properties (line-beginning-position)
                                                    (line-end-position)
                                                    (list 'lichess-game-id id
                                                          'mouse-face 'highlight
                                                          'help-echo "RET: open in browser"))
                               (when id
                                 (lichess-tv--fetch-game id chan-name marker)))))))

(defun lichess-tv--fmt-inline (name title rating)
  "Format short player NAME with TITLE and RATING."
  (if name
      (string-trim
       (format "%s%s%s"
               (or title "")
               (if title " " "")
               (if rating
                   (format "%s (%s)" name rating)
                 name)))
    "Anonymous"))

(defun lichess-tv--fetch-game (id chan-name marker)
  "Fetch full /api/game/{id} and update line."
  (let ((url (format "https://lichess.org/api/game/%s" id)))
    (lichess-core-fetch-json
     url
     (lambda (res)
       (let ((line (if (= (car res) 200)
                       (lichess-core-game-line (cdr res))
                     (format "id:%s (HTTP %s)" id (car res)))))
         (lichess-tv--update-line marker
                                  (format "%-12s  %s" chan-name line)
                                  id))))))

(defun lichess-tv--update-line (pos text &optional id)
  "Replace line at POS (marker or number) with TEXT; attach game ID props."
  (let ((buf (get-buffer lichess-tv--buf)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (buffer-read-only nil))
          (save-excursion
            (goto-char (if (markerp pos) (marker-position pos) pos))
            (let ((current-id (get-text-property (line-beginning-position) 'lichess-game-id)))
              (when (or (null id) (equal id current-id))
                (delete-region (line-beginning-position) (line-end-position))
                (goto-char (line-beginning-position))
                (insert text)
                (when id
                  (add-text-properties (line-beginning-position) (line-end-position)
                                       (list 'lichess-game-id id
                                             'mouse-face 'highlight
                                             'help-echo "RET: open in browser")))))))))))

;;;###autoload
(defun lichess-tv-debug (&optional channel)
  "Dump raw JSON for /api/tv/channels and (optionally) /api/game/{id} of CHANNEL."
  (interactive
   (list (let* ((chs '("best" "blitz" "bullet" "rapid" "classical" "bot"
                       "computer" "racingKings" "crazyhouse" "threeCheck"
                       "kingOfTheHill" "atomic" "horde" "chess960" "ultraBullet"))
                (ans (completing-read "Channel (empty = all): " chs nil nil "")))
           (if (string-empty-p ans) nil ans))))
  (let* ((buf  (get-buffer-create lichess-tv-debug--buf))
         tail)
    (with-current-buffer buf (lichess-core-mode))
    (lichess-core-with-buf buf
                           (lambda ()
                             (erase-buffer)
                             (insert "Fetching /api/tv/channels …\n\n")
                             (setq tail (copy-marker (point-max) t))))
    (pop-to-buffer buf)
    (cl-labels
        ((append-tail (&rest xs)
           (lichess-core-with-buf buf
                                  (lambda ()
                                    (goto-char (marker-position tail))
                                    (while xs
                                      (let ((x (pop xs)))
                                        (cond
                                         ((eq x :nl) (insert "\n"))
                                         ((eq x :hr) (insert (make-string 70 ?─) "\n"))
                                         ((eq x :ts) (insert (format-time-string "[%H:%M:%S] ")))
                                         ((eq x :pp) (pp (pop xs) (current-buffer)))
                                         ((stringp x) (insert x))
                                         (t (insert (format "%s" x))))))
                                    (insert "\n")
                                    (setq tail (copy-marker (point) t))))))
      ;; Get channels
      (lichess-core-fetch-json
       "https://lichess.org/api/tv/channels"
       (lambda (res)
         (let ((status (car res)) (data (cdr res)))
           (append-tail :hr :ts (format "HTTP %s /api/tv/channels" status)
                        :nl "--- RAW JSON ---" :nl :nl :pp data :hr :nl)
           ;; Get optional game of CHANNEL
           (when (and (= status 200) channel)
             (let* ((sym   (intern (downcase channel)))
                    (entry (alist-get sym data))
                    (gid   (and entry (or (alist-get 'gameId entry)
                                          (alist-get 'id entry)))))
               (append-tail (format "Channel %s -> game %s" channel (or gid "nil"))
                            :nl :hr :nl)
               (when gid
                 (lichess-core-fetch-json
                  (format "https://lichess.org/api/game/%s" gid)
                  (lambda (res2)
                    (append-tail :ts (format "HTTP %s /api/game/%s" (car res2) gid)
                                 :nl "--- RAW JSON ---" :nl :nl :pp (cdr res2) :hr :nl))))))))))))

(provide 'lichess-tv)
;;; lichess-tv.el ends here