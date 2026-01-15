;;; lichess-core.el --- Core utilities for Lichess.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025  Alexandr Timchenko
;; URL: https://github.com/tmythicator/Lichess.el
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
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
(require 'lichess-http)

(defvar lichess-core-mode-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m special-mode-map)
    ;; TODO: find out, what would be the core-keys for lichess.el navigation
    m)
  "Keymap for `lichess-core-mode'.")

(define-derived-mode
 lichess-core-mode
 special-mode
 "Lichess"
 "Base mode for Lichess buffers."
 (read-only-mode 1)
 (setq truncate-lines t))

;;;; Buffers
(defmacro lichess-core-with-buf (buf &rest body)
  "Eval BODY inside BUF with read-only protections disabled.
No-op if BUF is not live."
  (declare (indent 1) (debug (form body)))
  `(let ((--buf ,buf))
     (when (buffer-live-p --buf)
       (with-current-buffer --buf
         (let ((inhibit-read-only t))
           (save-excursion
             (save-restriction
               (widen)
               ,@body)))))))

(provide 'lichess-core)
;;; lichess-core.el ends here
