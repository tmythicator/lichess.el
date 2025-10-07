;;; lichess-core.el --- Core utilities for Lichess.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025  Alexandr Timchenko
;; SPDX-License-Identifier: GPL-3.0-or-later
;; See LICENSE for details.
;;
;;; Commentary:
;;
;; Internal shared functions: HTTP requests, JSON parsing,
;; buffer management and helper macros used across submodules.
;;
;;; Code:

(require 'url)
(require 'json)
(require 'cl-lib)

(defgroup lichess nil
  "Play and interact with Lichess."
  :group 'games)

(defvar lichess-core-mode-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m special-mode-map)
    (define-key m (kbd "RET")       #'lichess-core-open-at-point)
    (define-key m (kbd "<return>")  #'lichess-core-open-at-point)
    (define-key m (kbd "C-m")       #'lichess-core-open-at-point)
    m)
  "Keymap for `lichess-core-mode'.")

(define-derived-mode lichess-core-mode special-mode "Lichess"
  "Base mode for Lichess buffers."
  (read-only-mode 1)
  (setq truncate-lines t))

;;;; HTTP/JSON
(defun lichess-core-fetch-json (url cb &optional headers)
  "GET URL with HEADERS, parse JSON; call CB with (STATUS . JSON-or-nil)."
  (let ((url-request-method "GET")
        (url-request-extra-headers (or headers '(("Accept" . "application/json")))))
    (url-retrieve
     url
     (lambda (_)
       (let* ((status (or (bound-and-true-p url-http-response-status) 0))
              (_ (goto-char (or url-http-end-of-headers (point-min))))
              (body (buffer-substring-no-properties (point) (point-max)))
              (json (condition-case _
                        (let ((json-object-type 'alist)
                              (json-array-type 'list))
                          (json-read-from-string body))
                      (error nil))))
         (funcall cb (cons status json))))
     nil t)))

;;;; Buffers
(defun lichess-core-with-buf (buf fn)
  "Run FN in BUF, forcibly disabling any read-only protection."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (buffer-read-only nil))
        (save-excursion
          (save-restriction
            (widen)
            (funcall fn)))))))


(defun lichess-core-open-at-point ()
  "Open game at point by text-property 'lichess-game-id'."
  (interactive)
  (let* ((id (get-text-property (point) 'lichess-game-id)))
    (if id
        (browse-url (format "https://lichess.org/%s" id))
      (message "No game id on this line."))))

;; ---- JSON utils ----
(defun lichess-core-prop (alist &rest keys)
  "Safe nested alist get."
  (let ((x alist))
    (while (and keys x) (setq x (alist-get (pop keys) x))) x))

(defun lichess-core-fmt-player (side)
  "Return \"GM Name (2870)\" / \"Name (2870)\" / \"Anonymous\" from SIDE."
  (let* ((user   (or (alist-get 'user side) side))
         (title  (or (alist-get 'title user) (alist-get 'title side)))
         (name   (or (alist-get 'name user)
                     (alist-get 'username user)
                     (alist-get 'name side)
                     (alist-get 'username side)))
         (rating (or (alist-get 'rating side) (alist-get 'rating user))))
    (if name
        (string-trim
         (format "%s%s%s"
                 (or title "")
                 (if title " " "")
                 (if rating (format "%s (%s)" name rating) name)))
      "Anonymous")))

(defun lichess-core-game-line (j)
  "Build one-line game summary from /api/game/{id} JSON J."
  (let* ((id    (or (alist-get 'id j) (alist-get 'gameId j)))
         (white (or (lichess-core-fmt-player (lichess-core-prop j 'players 'white))
                    (lichess-core-fmt-player (alist-get 'white j))))
         (black (or (lichess-core-fmt-player (lichess-core-prop j 'players 'black))
                    (lichess-core-fmt-player (alist-get 'black j))))
         (speed (or (alist-get 'speed j) "")))
    (format "%-28s vs %-28s   id:%s   %s" white black id speed)))

(provide 'lichess-core)
;;; lichess-core.el ends here
