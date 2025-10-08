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
(require 'lichess-util)

(defcustom lichess-tv-refresh-seconds 5
  "Auto-refresh period for best view."
  :type 'integer :group 'lichess)

(defcustom lichess-tv-fetch-delay 0.12
  "Delay between successive /api/game requests (seconds) to avoid HTTP 429."
  :type 'number :group 'lichess)

(defvar lichess-tv--buf "*Lichess TV*")
(defvar lichess-tv-debug--buf "*(Debug) Lichess TV*")
(defvar lichess-tv--next-at 0.0)

;;;###autoload
(defun lichess-tv ()
  "Show current Lichess TV channels; lines are clickable (RET)."
  (interactive)
  (let ((buf (get-buffer-create lichess-tv--buf)))
    (with-current-buffer buf
      (lichess-core-mode)
      (local-set-key (kbd "g") #'lichess-tv)) ;; refresh
    (lichess-core-with-buf buf
      (erase-buffer)
      (insert "Fetching Lichess TV channels…\n"))
    (pop-to-buffer buf))
  (lichess-core-fetch-json "https://lichess.org/api/tv/channels"
                           #'lichess-tv--handle-channels))

(defun lichess-tv--update-line (pos text &optional id)
  "Replace line at POS with TEXT. If POS is stale, find the line by game ID."
  (let ((buf (get-buffer lichess-tv--buf)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (save-excursion
            ;; 1) try marker position
            (let* ((p (and (markerp pos) (marker-buffer pos)
                           (marker-position pos)))
                   bol cur-id)
              (when p
                (goto-char p)
                (setq bol (line-beginning-position)
                      cur-id (get-text-property bol 'lichess-game-id)))
              ;; 2) fallback: locate by ID across the buffer
              (when (and id (not (equal id cur-id)))
                (let ((hit (text-property-any (point-min) (point-max)
                                              'lichess-game-id id)))
                  (when hit
                    (goto-char hit)
                    (setq bol (line-beginning-position)
                          cur-id id))))
              ;; 3) update if we found a line
              (when bol
                (let ((eol (min (point-max) (1+ (line-end-position)))))
                  (delete-region bol eol)
                  (goto-char bol)
                  (lichess-util--insert-propertized-line text id))))))))))


(defun lichess-tv--fetch-game (id chan-name marker)
  "Throttled fetch of /api/game/{id} and update the line at MARKER."
  (let* ((now (float-time))
         (at  (max now lichess-tv--next-at)))
    (setq lichess-tv--next-at (+ at lichess-tv-fetch-delay))
    (run-at-time (- at now) nil
                 (lambda ()
                   (let ((url (format "https://lichess.org/api/game/%s" id)))
                     (lichess-core-fetch-json
                      url
                      (lambda (res)
                        (let ((status (car res))
                              (data   (cdr res)))
                          ;;                          (message "fetched: %s, time: %s" id at)
                          (if (= status 200)
                              (let ((vs (lichess-util--game->vs data)))
                                (lichess-tv--update-line
                                 marker
                                 (format "%-12s  %-64s  id:%s" chan-name vs id)
                                 id))
                            (lichess-tv--update-line
                             marker
                             (format "%-12s  id:%s (HTTP %s)" chan-name id status)
                             id))))))))))

(defun lichess-tv--insert-channel (pair)
  "Insert a placeholder for PAIR = (CHAN . GAME), then fetch full game and update."
  (pcase-let* ((`(,chan . ,g) pair)
               (chan-name (symbol-name chan))
               (id        (or (lichess-util--aget g 'gameId)
                              (lichess-util--aget g 'id))))
    (lichess-core-with-buf (get-buffer lichess-tv--buf)
      (goto-char (point-max))
      (let* ((text   (format "%-12s  %s  id:%s" chan-name "loading…" id))
             (marker (lichess-util--insert-propertized-line text id)))
        (when id
          (lichess-tv--fetch-game id chan-name marker))))))

(defun lichess-tv--handle-channels (res)
  "Process /api/tv/channels response."
  (if (/= (car res) 200)
      (lichess-core-with-buf (get-buffer lichess-tv--buf)
        (erase-buffer)
        (insert (format "HTTP %s from /api/tv/channels\n" (car res))))
    (mapc #'lichess-tv--insert-channel (cdr res))))

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
      (erase-buffer)
      (insert "Fetching /api/tv/channels …\n\n")
      (setq tail (copy-marker (point-max) t)))
    (pop-to-buffer buf)
    (cl-labels
        ((append-tail (&rest xs)
           (lichess-core-with-buf buf
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
             (setq tail (copy-marker (point) t)))))
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