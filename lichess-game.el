;;; lichess-game.el --- Game stream/debug utilities for Lichess -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025  Alexandr Timchenko
;; SPDX-License-Identifier: GPL-3.0-or-later
;; See LICENSE for details.
;;
;;; Commentary:
;; Minimal game streaming/debug using NDJSON:
;; - M-x lichess-game-watch         -> watch a game with live board and history navigation
;; - M-x lichess-game-stream-debug  -> pretty-print /api/stream/game/{id}
;; - M-x lichess-game-stream-stop   -> close the current stream
;;
;;; Code:

(require 'lichess-core)
(require 'lichess-util)

(defvar lichess-game--stream nil
  "Current `lichess-http-stream' for a game NDJSON, or nil when not running.")

(defun lichess-game--stream-buffer-name (id)
  (format "*Lichess Game Stream: %s*" id))

(defvar lichess-game--debug-id "t7HAF0vX"
  "Default game ID used for debugging.")

(defvar-local lichess-game--fen-history nil
  "Buffer-local vector of FEN strings for the current game.")
(defvar-local lichess-game--eval-cache nil
  "Buffer-local hash table caching evaluations: index -> eval-string.")
(defvar-local lichess-game--current-idx nil
  "Buffer-local index for `lichess-game--fen-history'.")
(defvar-local lichess-game--live-mode t
  "Buffer-local flag. When non-nil, board updates automatically.")
(defvar-local lichess-game--perspective 'white
  "The current board perspective. Can be 'white, 'black or 'auto.")

(defvar lichess-game-buffer-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "q") #'lichess-game-stream-stop)
    (define-key m (kbd "p") #'lichess-game-history-previous)
    (define-key m (kbd "n") #'lichess-game-history-next)
    (define-key m (kbd "v") #'lichess-game-set-perspective)
    (define-key m (kbd "l") #'lichess-game-evaluate-history)
    m))

(define-derived-mode lichess-game-buffer-mode special-mode "Lichess-Game"
  "Major mode for live Lichess game buffers."
  (setq truncate-lines t))

(defun lichess-game--render-pos (pos &optional eval-str)
  "Render the given POS struct in the current buffer with EVAL-STR."
  (erase-buffer)
  (insert (lichess-fen-render-heading pos "org+unicode" lichess-game--perspective))
  (insert (lichess-fen-render-org-table pos t lichess-game--perspective eval-str))
  (insert (format "\n\nPosition %d/%d" (1+ lichess-game--current-idx) (length lichess-game--fen-history)))
  (when (fboundp 'org-table-align)
    (save-excursion
      (goto-char (point-min))
      (org-table-align)))
  (goto-char (point-min)))

(defun lichess-game-history-previous ()
  "Move to the previous position in game history."
  (interactive)
  (when (and lichess-game--current-idx (> lichess-game--current-idx 0))
    (setq lichess-game--live-mode nil)
    (setq lichess-game--current-idx (1- lichess-game--current-idx))
    (let* ((fen (aref lichess-game--fen-history lichess-game--current-idx))
           (eval-str (gethash lichess-game--current-idx lichess-game--eval-cache))
           (pos (ignore-errors (lichess-chess-parse-fen fen))))
      (when pos
        (lichess-core-with-buf (current-buffer)
          (lichess-game--render-pos pos eval-str))))
    (message "Position %d/%d"
             (1+ lichess-game--current-idx)
             (length lichess-game--fen-history))))

(defun lichess-game-history-next ()
  "Move to the next position in game history."
  (interactive)
  (when (and lichess-game--current-idx (< lichess-game--current-idx (1- (length lichess-game--fen-history))))
    (setq lichess-game--current-idx (1+ lichess-game--current-idx))
    (let* ((fen (aref lichess-game--fen-history lichess-game--current-idx))
           (eval-str (gethash lichess-game--current-idx lichess-game--eval-cache))
           (pos (ignore-errors (lichess-chess-parse-fen fen))))
      (when pos
        (lichess-core-with-buf (current-buffer)
          (lichess-game--render-pos pos eval-str))))
    (when (= lichess-game--current-idx (1- (length lichess-game--fen-history)))
      (setq lichess-game--live-mode t))
    (message "Position %d/%d %s"
             (1+ lichess-game--current-idx)
             (length lichess-game--fen-history)
             (if lichess-game--live-mode "(Live)" ""))))

(defun lichess-game--extract-fen (obj)
  "Extract FEN from an NDJSON event (from obj.fen or obj.state.fen)."
  (or (lichess-util--aget obj 'fen)
      (let ((st (lichess-util--aget obj 'state)))
        (and (consp st) (lichess-util--aget st 'fen)))))

(defun lichess-game--fen-history-vpush (fen)
  "Append FEN to the lichess-game--fen-history vector."
  (let* ((len (length lichess-game--fen-history))
         (last (and (> len 0) (aref lichess-game--fen-history (1- len)))))
    (unless (and last (string= last fen))
      (setq lichess-game--fen-history
            (vconcat lichess-game--fen-history (vector fen))))))

(defun lichess-game--reset-local-vars ()
  "Reset buffer-local state variables for a new game."
  (setq-local lichess-game--fen-history (make-vector 0 t))
  (setq-local lichess-game--eval-cache (make-hash-table))
  (setq-local lichess-game--current-idx -1)
  (setq-local lichess-game--live-mode t)
  (setq-local lichess-game--perspective 'white))

;;;###autoload
(defun lichess-game-watch (id)
  "Watch a Lichess game by ID with a real-time board display and history."
  (interactive
   (list
    (read-string "Lichess game ID to watch: " nil nil lichess-game--debug-id)))
  (let* ((buf-name (lichess-game--stream-buffer-name id))
         (buf (get-buffer-create buf-name)))
    (lichess-game-stream-stop)

    (setq lichess-game--stream
          (lichess-http-ndjson-open
           (format "/api/stream/game/%s" id)
           :buffer-name (buffer-name buf)
           :on-open
           (lambda (_proc _buf)
             (lichess-core-with-buf buf
               (unless (eq major-mode 'lichess-game-buffer-mode)
                 (lichess-game-buffer-mode))
               (lichess-game--reset-local-vars)
               (erase-buffer)
               (insert (format "Connecting to game %s…\n" id))))
           :on-event
           (lambda (obj)
             (lichess-core-with-buf buf
               (unless (lichess-util--aget obj 'id)
                 (when-let ((fen (lichess-game--extract-fen obj)))
                   (lichess-game--fen-history-vpush fen)
                   (when lichess-game--live-mode
                     (setq lichess-game--current-idx (1- (length lichess-game--fen-history)))
                     (when-let ((pos (ignore-errors (lichess-chess-parse-fen fen))))
                       (lichess-game--render-pos pos)))))))
           :on-close
           (lambda (_proc msg)
             (when (buffer-live-p buf)
               (lichess-core-with-buf buf
                 (goto-char (point-max))
                 (insert (format "\n[%s] — Stream disconnected: %s\n"
                                 (format-time-string "%H:%M:%S")
                                 (string-trim msg))))))))
    ;; 4. Finally, we show the buffer.
    (pop-to-buffer buf)))

;;;###autoload
(defun lichess-game-evaluate-history ()
  "Fetch cloud evaluations for all moves in the current game history."
  (interactive)
  (let ((game-buf (current-buffer)))
    (message "Batch fetching evaluations for %d moves..." (length lichess-game--fen-history))
    (dotimes (i (length lichess-game--fen-history))
      (let ((cached-eval (gethash i lichess-game--eval-cache)))
        (when (or (null cached-eval)
                  (string= cached-eval "..."))
          (let* ((fen (aref lichess-game--fen-history i))
                 (pos (ignore-errors (lichess-chess-parse-fen fen))))
            (when pos
              (puthash i "..." lichess-game--eval-cache)
              (lichess-util-fetch-evaluation
               fen
               (lambda (eval-str)
                 (lichess-core-with-buf game-buf
                   (puthash i eval-str lichess-game--eval-cache)
                   (when (= i lichess-game--current-idx)
                     (lichess-game--render-pos pos eval-str))))))))))))

;;;###autoload
(defun lichess-game-set-perspective ()
  "Set the board perspective for the current game buffer."
  (interactive)
  (let* ((choices '("auto" "white" "black"))
         (new-persp-str (completing-read "Set perspective: " choices nil t nil nil "auto"))
         (new-persp (intern new-persp-str)))
    (setq lichess-game--perspective new-persp)
    ;; Re-render the current position with the new perspective
    (when lichess-game--current-idx
      (let* ((fen (aref lichess-game--fen-history lichess-game--current-idx))
             (pos (ignore-errors (lichess-chess-parse-fen fen)))
             (eval-str (gethash lichess-game--current-idx lichess-game--eval-cache)))
        (when pos (lichess-core-with-buf (current-buffer) (lichess-game--render-pos pos eval-str)))))))

;;;###autoload
(defun lichess-game-stream-debug (id)
  "Open NDJSON stream for game ID at /api/stream/game/{id} and pretty-print events."
  (interactive
   (list
    (read-string
     (format "Lichess game id (default %s): " lichess-game--debug-id)
     nil nil lichess-game--debug-id)))
  (let* ((buf (get-buffer-create (lichess-game--stream-buffer-name id))))
    (with-current-buffer buf (lichess-core-mode))
    ;; Close previous stream if any
    (when lichess-game--stream
      (lichess-http-ndjson-close lichess-game--stream)
      (setq lichess-game--stream nil))
    ;; Open a new NDJSON stream via lichess-http
    (setq lichess-game--stream
          (lichess-http-ndjson-open
           (format "/api/stream/game/%s" id)
           :buffer-name (buffer-name buf)
           :on-open (lambda (_proc _buf)
                      (lichess-core-with-buf buf
                        (erase-buffer)
                        (insert (format "Connecting NDJSON stream for game %s…\n\n" id))))
           :on-event (lambda (obj)
                       (lichess-core-with-buf buf
                         (goto-char (point-max))
                         (insert (format "[%s] — NDJSON event\n"
                                         (format-time-string "%H:%M:%S")))
                         (pp obj (current-buffer))
                         (insert "\n")))
           :on-close (lambda (_proc msg)
                       (lichess-core-with-buf buf
                         (goto-char (point-max))
                         (insert (format "[%s] — %s\n"
                                         (format-time-string "%H:%M:%S")
                                         (string-trim msg)))))))
    (pop-to-buffer buf)))

;;;###autoload
(defun lichess-game-stream-stop ()
  "Stop the current Lichess NDJSON game stream (if any)."
  (interactive)
  (if lichess-game--stream
      (progn
        (lichess-http-ndjson-close lichess-game--stream)
        (setq lichess-game--stream nil)
        (message "Lichess NDJSON stream stopped."))
    (message "No active Lichess NDJSON stream.")))

(provide 'lichess-game)
;;; lichess-game.el ends here
