;;; lichess-core.el --- Core utilities for Lichess.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026  Alexandr Timchenko
;; URL: https://github.com/tmythicator/Lichess.el
;; Version: 0.2
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
(require 'cl-lib)
(require 'lichess-http)

(cl-defstruct
 lichess-pos "Internal chess position.  Row 0 = rank 8 (top)."
 board ;; vector[8] of vector[8] chars
 stm ;; 'w or 'b
 castle ;; string like "KQkq" or "-"
 ep ;; (row . col) or nil
 halfmove ;; int
 fullmove) ;; int

(cl-defstruct
 lichess-game "State for a Lichess game."
 id ;; string
 variant ;; string (e.g. "standard", "chess960")
 initial-fen ;; string
 initial-pos ;; lichess-pos
 white ;; player-info (alist/plist)
 black ;; player-info (alist/plist)
 my-color ;; 'white, 'black, or nil
 fen-history ;; vector of strings
 eval-cache ;; hash-table
 current-idx ;; int
 live-mode ;; bool
 perspective ;; 'white, 'black, or 'auto
 speed ;; string (e.g. "blitz")
 status) ;; string (e.g. "started")

(defvar lichess-core-mode-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m special-mode-map)
    ;; TODO: find out, what would be the core-keys for lichess.el navigation
    m)
  "Keymap for `lichess-core-mode'.")

(defgroup lichess-core nil
  "Core utilities for Lichess."
  :group 'lichess)

(defcustom lichess-core-chess-font nil
  "Font family for Unicode chess pieces in the GUI.
If nil, use `fixed-pitch' family.  Example: \"DejaVu Sans\"."
  :set
  (lambda (symbol value)
    (set-default symbol value)
    (when (fboundp 'lichess-core-setup-chess-font)
      (lichess-core-setup-chess-font value)))
  :type '(choice (const :tag "Default" nil) string)
  :group 'lichess-core)

(defface lichess-core-board-face
  '((((type graphic)) :inherit fixed-pitch) (t :inherit fixed-pitch))
  "Face applied to the whole chess board."
  :group 'lichess-core)

(defun lichess-core-setup-chess-font (&optional font-family)
  "Force a monospaced font family for the whole board and pieces.
Apply FONT-FAMILY to the chess piece Unicode range (U+2654-U+265F).
If FONT-FAMILY is nil, try to find a suitable monospaced font.
This is useful if your default font makes the GUI board look jagged."
  (interactive (list
                (let ((all (font-family-list)))
                  (completing-read "Mono Font: " all
                                   nil t
                                   (or (car
                                        (seq-filter
                                         (lambda (f)
                                           (string-match-p "Mono" f))
                                         all))
                                       lichess-core-chess-font)))))
  (when (display-graphic-p)
    (let ((family
           (or font-family
               lichess-core-chess-font
               (face-attribute 'fixed-pitch :family)
               "monospace")))
      (set-fontset-font t '(#x2654 . #x265F) family)
      (set-fontset-font t '(#x00b7 . #x00b7) family)
      ;; Force the board face to use this family too
      (set-face-attribute 'lichess-core-board-face nil :family family)
      (message "Lichess: Board and chess font forced to %s" family))))

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
