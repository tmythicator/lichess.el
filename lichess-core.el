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

(defun lichess-core-open-at-point ()
  "Open game at point by text-property 'lichess-game-id'."
  (interactive)
  (let* ((id (get-text-property (point) 'lichess-game-id)))
    (if id
        (browse-url (format "https://lichess.org/%s" id))
      (message "No game id on this line."))))

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
(defmacro lichess-core-with-buf (buf &rest body)
  "Eval BODY inside BUF with read-only protections disabled.
No-op if BUF is not live."
  (declare (indent 1) (debug (form body)))
  `(let ((--buf ,buf))
     (when (buffer-live-p --buf)
       (with-current-buffer --buf
         (let ((inhibit-read-only t)
               (buffer-read-only nil))
           (save-excursion
             (save-restriction
               (widen)
               ,@body)))))))

(provide 'lichess-core)
;;; lichess-core.el ends here
