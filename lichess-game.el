;;; lichess-game.el --- Game stream/debug utilities for Lichess -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026  Alexandr Timchenko
;; URL: https://github.com/tmythicator/Lichess.el
;; Version: 0.3
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; See LICENSE for details.
;;
;;; Commentary:
;; Minimal game streaming/debug using NDJSON:
;; - M-x lichess-game-watch         -> watch a game with live board and history navigation
;; - M-x lichess-game-play          -> play/watch your own game (Real-time)
;; - M-x lichess-game-stream-debug  -> pretty-print /api/stream/game/{id}
;; - M-x lichess-game-stream-stop   -> close the current stream
;;
;; Note: Use `lichess-game-play' for zero-latency updates on your own games.
;;
;;; Code:

(require 'lichess-core)
(require 'lichess-util)
(require 'lichess-fen)
(require 'lichess-board)

(defvar lichess-game--stream nil
  "Current `lichess-http-stream' for a game NDJSON, or nil when not running.")

(defun lichess-game--stream-buffer-name (id)
  "Return buffer name for game ID."
  (format "*Lichess Game Stream: %s*" id))

(defvar-local lichess-game--state nil
  "Buffer-local `lichess-game` struct containing all game state.")

(defvar lichess-game-buffer-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "q") #'lichess-game-stream-stop)
    (define-key m (kbd "p") #'lichess-game-history-previous)
    (define-key m (kbd "n") #'lichess-game-history-next)
    (define-key m (kbd "v") #'lichess-game-set-perspective)
    (define-key m (kbd "l") #'lichess-game-evaluate-history)
    (define-key m (kbd "m") #'lichess-game-move)
    (define-key m (kbd "R") #'lichess-game-resign)
    (define-key m (kbd "D") #'lichess-game-draw)
    (define-key m (kbd "K") #'lichess-game-castle-kingside)
    (define-key m (kbd "Q") #'lichess-game-castle-queenside)
    (define-key m [mouse-1] #'lichess-game-mouse-handler)
    m))

(define-derived-mode
 lichess-game-buffer-mode
 special-mode
 "Lichess-Game"
 "Major mode for live Lichess game buffers."
 (setq truncate-lines t))

(defun lichess-game-render ()
  "Render the complete game view for the current game state.

Draws the header, board, evaluation bar (if available), and footer info.
All parameters are derived from the buffer-local `lichess-game--state'."
  (when-let* ((state lichess-game--state)
              (idx (lichess-game-current-idx state))
              (hist (lichess-game-fen-history state)))
    (let* ((fen (aref hist idx))
           (eval-str (gethash idx (lichess-game-eval-cache state)))
           (pos
            (ignore-errors
              (lichess-fen-parse fen)))
           (perspective (lichess-game-perspective state))
           (pos-info (format "Position %d/%d" (1+ idx) (length hist)))
           (highlights
            (when-let ((sq (lichess-game-selected-square state)))
              (list sq))))
      (when pos
        (lichess-board-render-to-buffer
         pos perspective eval-str pos-info highlights)))))

(defun lichess-game-history-previous ()
  "Move to the previous position in game history."
  (interactive)
  (when-let* ((state lichess-game--state)
              (idx (lichess-game-current-idx state))
              (hist (lichess-game-fen-history state)))
    (when (> idx 0)
      (setf (lichess-game-live-mode state) nil)
      (setf (lichess-game-current-idx state) (1- idx))
      (lichess-game-render))))

(defun lichess-game-history-next ()
  "Move to the next position in game history."
  (interactive)
  (when-let* ((state lichess-game--state)
              (idx (lichess-game-current-idx state))
              (hist (lichess-game-fen-history state)))
    (when (< idx (1- (length hist)))
      (setf (lichess-game-current-idx state) (1+ idx))
      (when (= (lichess-game-current-idx state) (1- (length hist)))
        (setf (lichess-game-live-mode state) t))
      (lichess-game-render))))

(defun lichess-game--extract-fen (obj)
  "Extract FEN from an NDJSON event (from OBJ.fen or OBJ.state.fen)."
  (or (lichess-util--aget obj 'fen)
      (let ((st (lichess-util--aget obj 'state)))
        (and (consp st) (lichess-util--aget st 'fen)))))


(defun lichess-game--get-fen-fullmove (fen)
  "Extract the fullmove number (last field) from FEN."
  (let ((parts (split-string fen " ")))
    (if (>= (length parts) 6)
        (string-to-number (car (last parts)))
      1)))

(defun lichess-game--fen-history-vpush (fen)
  "Append FEN to the `lichess-game--fen-history' via STATE.
Handles stream replays by resetting history if a move regression is detected
immediately after the initial summary state."
  (when-let* ((state lichess-game--state)
              (hist (lichess-game-fen-history state)))
    (let* ((len (length hist))
           (last (and (> len 0) (aref hist (1- len)))))
      ;; Detect Replay: If history has 1 item (Summary) and new FEN is earlier, reset.
      (when (and (= len 1) last)
        (let ((last-fm (lichess-game--get-fen-fullmove last))
              (new-fm (lichess-game--get-fen-fullmove fen)))
          (when (< new-fm last-fm)
            (setf hist (vector))
            (setf (lichess-game-fen-history state) hist))))

      (unless (and last (string= last fen))
        (setf (lichess-game-fen-history state)
              (vconcat hist (vector fen)))))))

(defun lichess-game--reset-local-vars ()
  "Reset buffer-local state variables for a new game."
  (setq-local lichess-game--state
              (make-lichess-game
               :fen-history (make-vector 0 t)
               :eval-cache (make-hash-table)
               :current-idx -1
               :live-mode t
               :perspective 'white
               :selected-square nil)))

(defun lichess-game--fmt-player-name (user-obj)
  "Extract a readable name from a Lichess player/user USER-OBJ."
  (or (lichess-util--aget user-obj 'name)
      (lichess-util--aget user-obj 'username)
      (lichess-util--aget user-obj 'id)
      (when-let ((lvl (lichess-util--aget user-obj 'aiLevel)))
        (format "Stockfish level %d" lvl))
      (when-let ((ai (lichess-util--aget user-obj 'ai)))
        (format "Stockfish level %d" ai))
      "Anonymous"))

(defun lichess-game--stream-on-open (id buf msg-prefix _proc _buf)
  "Callback for NDJSON stream open.
ID is the game, BUF is the buffer, MSG-PREFIX for status."
  (lichess-core-with-buf
   buf
   (unless (eq major-mode 'lichess-game-buffer-mode)
     (lichess-game-buffer-mode))
   (lichess-game--reset-local-vars)
   (setf (lichess-game-id lichess-game--state) id)
   (erase-buffer)
   (insert (format "%s %s…\n" msg-prefix id))))

(defun lichess-game--stream-on-event (buf obj)
  "Callback for spectator NDJSON stream events (contain FEN).
BUF is the target buffer, OBJ is the parsed JSON event."
  (lichess-core-with-buf
   buf
   (let ((fen (lichess-game--extract-fen obj))
         (white (lichess-util--aget obj 'white))
         (black (lichess-util--aget obj 'black))
         (state lichess-game--state))
     ;; Update names if found (usually in the first message)
     (when (or white black)
       (let ((w-name (lichess-game--fmt-player-name white))
             (b-name (lichess-game--fmt-player-name black)))
         (setf
          (lichess-game-white state) white
          (lichess-game-black state) black)
         (goto-char (point-max))
         (insert
          (format "\nWatching: %s (W) vs %s (B)\n" w-name b-name))))
     ;; Render FEN
     (when fen
       (lichess-game--fen-history-vpush fen)
       (when (lichess-game-live-mode state)
         (setf (lichess-game-current-idx state)
               (1- (length (lichess-game-fen-history state))))
         (lichess-game-render))))))

(defun lichess-game--board-on-event (buf obj)
  "Callback for Board API stream events.
BUF is the target buffer, OBJ is the parsed JSON event."
  (lichess-core-with-buf
   buf
   (let* ((type (lichess-util--aget obj 'type))
          (state-obj (or (lichess-util--aget obj 'state) obj))
          (moves (lichess-util--aget state-obj 'moves))
          (white (lichess-util--aget obj 'white))
          (black (lichess-util--aget obj 'black))
          (state lichess-game--state))

     ;; 1. Handle game setup
     (when (string= type "gameFull")
       (let* ((ifen
               (or (lichess-util--aget obj 'initialFen) "startpos"))
              (ipos (lichess-fen-parse ifen))

              (variant
               (lichess-util--aget
                (lichess-util--aget obj 'variant) 'key))
              (w-name (lichess-game--fmt-player-name white))
              (b-name (lichess-game--fmt-player-name black))
              (w-id (lichess-util--aget white 'id))
              (b-id (lichess-util--aget black 'id))
              ;; Determine my color by matching username/ID
              (curr-user
               (and (boundp 'lichess-username) lichess-username))
              (am-white
               (or (and curr-user
                        (string-match-p curr-user (or w-name "")))
                   (and w-id curr-user (string= w-id curr-user))))
              (am-black
               (or (and curr-user
                        (string-match-p curr-user (or b-name "")))
                   (and b-id curr-user (string= b-id curr-user))))
              (my-col
               (cond
                (am-white
                 'white)
                (am-black
                 'black)
                (t
                 nil))))

         (setf
          (lichess-game-initial-pos state) ipos
          (lichess-game-variant state) variant
          (lichess-game-white state) white
          (lichess-game-black state) black
          (lichess-game-my-color state) my-col
          (lichess-game-id state) (lichess-util--aget obj 'id))
         (goto-char (point-max))
         (insert
          (format
           "\nGame (Board API): %s (W) vs %s (B) [%s] (You: %s)\n"
           w-name b-name variant (or my-col "Spectator")))))

     ;; 2. Reconstruct FEN and update history
     (when-let ((ipos (lichess-game-initial-pos state)))
       (let* ((current-pos (lichess-fen-apply-moves ipos moves))
              (current-fen (lichess-fen-pos->fen current-pos)))
         (lichess-game--fen-history-vpush current-fen)
         (when (lichess-game-live-mode state)
           (setf (lichess-game-current-idx state)
                 (1- (length (lichess-game-fen-history state))))
           (lichess-game-render)))))))

(defun lichess-game--stream-on-close (buf msg-prefix _proc msg)
  "Callback for NDJSON stream close.
BUF, MSG-PREFIX, and MSG describe the event."
  (when (buffer-live-p buf)
    (lichess-core-with-buf
     buf (goto-char (point-max))
     (insert
      (format "\n[%s] — %s disconnected: %s\n"
              (format-time-string "%H:%M:%S")
              msg-prefix
              (string-trim msg))))))

;;;###autoload
(defun lichess-game-watch (id)
  "Watch a Lichess game by ID with a real-time board display and history."
  (interactive "sLichess game ID to watch: ")
  (let* ((buf-name (lichess-game--stream-buffer-name id))
         (buf (get-buffer-create buf-name)))
    (lichess-game-stream-stop)

    (setq lichess-game--stream
          (lichess-http-ndjson-open
           (format "/api/stream/game/%s" id)
           :buffer-name (buffer-name buf)
           :on-open
           (apply-partially #'lichess-game--stream-on-open
                            id buf "Connecting to game (Spectator)")
           :on-event
           (apply-partially #'lichess-game--stream-on-event buf)
           :on-close
           (apply-partially #'lichess-game--stream-on-close
                            buf "Stream")))
    (pop-to-buffer buf)))

;;;###autoload
(defun lichess-game-play (id)
  "Watch a game you are playing using the Board API for zero-delay update.
This uses /api/board/game/stream/{ID} which has no delay."
  (interactive "sGame ID: ")
  (let* ((buf-name (lichess-game--stream-buffer-name id))
         (buf (get-buffer-create buf-name)))
    (lichess-game-stream-stop)
    (setq lichess-game--stream
          (lichess-http-ndjson-open
           (format "/api/board/game/stream/%s" id)
           :buffer-name (buffer-name buf)
           :on-open
           (apply-partially
            #'lichess-game--stream-on-open
            id buf "Connecting to your game (Zero-delay)")
           :on-event
           (apply-partially #'lichess-game--board-on-event buf)
           :on-close
           (apply-partially #'lichess-game--stream-on-close
                            buf "Zero-delay stream")))
    (pop-to-buffer buf)))

;;;###autoload
(defun lichess-game-evaluate-history ()
  "Fetch cloud evaluations for all positions in the current game history."
  (interactive)
  (let ((game-buf (current-buffer))
        (state lichess-game--state))
    (when state
      (let ((hist (lichess-game-fen-history state)))
        (message "Batch fetching evaluations for %d moves..."
                 (length hist))
        (dotimes (i (length hist))
          (let ((cached-eval
                 (gethash i (lichess-game-eval-cache state))))
            (when (or (null cached-eval)
                      (string-empty-p cached-eval)
                      (string= cached-eval "..."))
              (let* ((fen (aref hist i))
                     (pos
                      (ignore-errors
                        (lichess-fen-parse fen))))
                (when pos
                  (puthash i "..." (lichess-game-eval-cache state))
                  (lichess-util-fetch-evaluation
                   fen
                   (lambda (eval-str)
                     (lichess-core-with-buf
                      game-buf
                      (puthash
                       i eval-str (lichess-game-eval-cache state))
                      (when (= i (lichess-game-current-idx state))
                        (lichess-game-render))))))))))))))

;;;###autoload
(defun lichess-game-set-perspective ()
  "Set the board perspective for the current game buffer."
  (interactive)
  (when-let* ((state lichess-game--state)
              (idx (lichess-game-current-idx state))
              (hist (lichess-game-fen-history state)))
    (let* ((choices '("auto" "white" "black"))
           (new-persp-str
            (completing-read "Set perspective: " choices
                             nil
                             t
                             nil
                             nil
                             "auto"))
           (new-persp (intern new-persp-str)))
      (setf (lichess-game-perspective state) new-persp)
      (when idx
        (lichess-core-with-buf
         (current-buffer) (lichess-game-render))))))

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

;;;###autoload
(defun lichess-game-move (move)
  "Make a move in the current game.
MOVE should be in UCI format (e.g., e2e4)."
  (interactive "sMove (UCI, e.g. e2e4): ")
  (unless (and lichess-game--state
               (lichess-game-id lichess-game--state))
    (error "No game ID found for this buffer"))
  (let ((game-id (lichess-game-id lichess-game--state)))
    (message "Sending move %s for game %s..." move game-id)
    (lichess-http-request
     (format "/api/board/game/%s/move/%s" game-id move)
     (lambda (res)
       (let ((status (car res))
             (json (cdr res)))
         (if (= status 200)
             (message "Move %s sent successfully" move)
           (message "Error sending move: %d %s"
                    status
                    (or (lichess-util--aget json 'error) "")))))
     :method "POST")))

;;;###autoload
(defun lichess-game-resign ()
  "Resign the current game."
  (interactive)
  (unless (and lichess-game--state
               (lichess-game-id lichess-game--state))
    (error "No game ID found for this buffer"))
  (when (yes-or-no-p "Really resign this game? ")
    (let ((game-id (lichess-game-id lichess-game--state)))
      (message "Resigning game %s..." game-id)
      (lichess-http-request
       (format "/api/board/game/%s/resign" game-id)
       (lambda (res)
         (let ((status (car res))
               (json (cdr res)))
           (if (= status 200)
               (message "Game resigned.")
             (message "Error resigning: %d %s"
                      status
                      (or (lichess-util--aget json 'error) "")))))
       :method "POST"))))

;;;###autoload
(defun lichess-game-draw ()
  "Propose or accept a draw in the current game."
  (interactive)
  (unless (and lichess-game--state
               (lichess-game-id lichess-game--state))
    (error "No game ID found for this buffer"))
  (when (yes-or-no-p "Offer/Accept draw? ")
    (let ((game-id (lichess-game-id lichess-game--state)))
      (message "Sending draw request for game %s..." game-id)
      (lichess-http-request
       (format "/api/board/game/%s/draw/yes" game-id)
       (lambda (res)
         (let ((status (car res))
               (json (cdr res)))
           (if (= status 200)
               (message "Draw request sent/accepted.")
             (message "Error with draw request: %d %s"
                      status
                      (or (lichess-util--aget json 'error) "")))))
       :method "POST"))))

(defun lichess-game--get-castling-move (color side)
  "Return UCI castling move string for COLOR and SIDE."
  (when-let* ((state lichess-game--state)
              (variant (or (lichess-game-variant state) "standard"))
              (initial-pos (lichess-game-initial-pos state)))
    (let ((is-960 (string= variant "chess960"))
          (row
           (if (eq color 'white)
               7
             0)))

      (if (not is-960)
          ;; Standard Chess
          (pcase (cons color side)
            (`(white . kingside) "e1g1")
            (`(white . queenside) "e1c1")
            (`(black . kingside) "e8g8")
            (`(black . queenside) "e8c8"))

        ;; Chess960 Logic
        (when-let* ((board (lichess-pos-board initial-pos))
                    (row-vec (aref board row))
                    (king-col
                     (cl-position
                      (if (eq color 'white)
                          ?K
                        ?k)
                      row-vec))
                    (rook-col
                     (if (eq side 'kingside)
                         ;; Find rightmost rook
                         (cl-position
                          (if (eq color 'white)
                              ?R
                            ?r)
                          row-vec
                          :from-end t)
                       ;; Find leftmost rook
                       (cl-position
                        (if (eq color 'white)
                            ?R
                          ?r)
                        row-vec))))
          (let ((k-sq (format "%c%d" (+ ?a king-col) (- 8 row)))
                (r-sq (format "%c%d" (+ ?a rook-col) (- 8 row))))
            (concat k-sq r-sq)))))))

(defun lichess-game--do-castle (side)
  "Perform castling for SIDE (symbols `kingside' or `queenside')."
  (unless (and lichess-game--state
               (lichess-game-id lichess-game--state))
    (error "No game ID"))
  (let* ((state lichess-game--state)
         (hist (lichess-game-fen-history state))
         (last-fen
          (and (> (length hist) 0) (aref hist (1- (length hist)))))
         (pos (and last-fen (lichess-fen-parse last-fen)))
         (my-color (lichess-game-my-color state))
         (turn (and pos (lichess-pos-stm pos)))
         (fen-color
          (if (eq turn 'w)
              'white
            'black))
         (color (or my-color fen-color))
         (move (lichess-game--get-castling-move color side)))

    (message
     "[DEBUG] castle: my-color=%S turn=%S fen-color=%S final-color=%S move=%S"
     my-color turn fen-color color move)

    (if move
        (if (and my-color (not (eq my-color fen-color)))
            (message "It is not your turn! (You are %s, turn is %s)"
                     my-color
                     fen-color)
          (lichess-game-move move))
      (message "Cannot determine castling move."))))

;;;###autoload
(defun lichess-game-castle-kingside ()
  "Castle Kingside (O-O)."
  (interactive)
  (lichess-game--do-castle 'kingside))

;;;###autoload
(defun lichess-game-castle-queenside ()
  "Castle Queenside (O-O-O)."
  (interactive)
  (lichess-game--do-castle 'queenside))

(defun lichess-game-handle-click (coord)
  "Handle a click on COORD (symbol like `e4') in the current game."
  (when-let* ((state lichess-game--state)
              (selected (lichess-game-selected-square state)))
    (cond
     ;; 1. Clicked the same square -> Deselect
     ((eq coord selected)
      (setf (lichess-game-selected-square state) nil)
      (lichess-game-render))

     ;; 2. Clicked a different square -> Attempt Move
     (t
      (let* ((move-str (format "%s%s" selected coord)))
        (setf (lichess-game-selected-square state) nil)
        (lichess-game-render)
        (lichess-game-move move-str)))))

  ;; If nothing selected, select the clicked square
  (unless (and lichess-game--state
               (lichess-game-selected-square lichess-game--state))
    (when lichess-game--state
      (setf (lichess-game-selected-square lichess-game--state) coord)
      (lichess-game-render))))

(defun lichess-game-mouse-handler (event)
  "Handle mouse clicks on the board.
EVENT is the mouse event."
  (interactive "e")
  (let*
      ((posn (event-start event))
       (obj (posn-object posn))
       (xy (posn-object-x-y posn)) ;; (x . y) relative to object (image)
       (col-row
        (when (imagep obj)
          ;; GUI (SVG image)
          (let*
              ((x (car xy))
               (y (cdr xy))
               (sq-size 45) ;; hardcoded for now, matches lichess-board-gui
               (c (floor (/ x sq-size)))
               (r (floor (/ y sq-size))))
            (cons c r))))) ;; (col . row) 0-7

    (when col-row
      (let* ((c (car col-row))
             (r (cdr col-row))
             (state lichess-game--state)
             (persp (or (lichess-game-perspective state) 'white))
             ;; Adjust for perspective
             ;; If black perspective, board is flipped.
             ;; Visually (0,0) is top-left.
             ;; White persp: (0,0) is a8. r=0->8. c=0->a. Correct?
             ;; No, standard: r=0 is rank 8. c=0 is file a.
             ;; lichess-board-gui: (x=0,y=0) -> (col=0, row=0) -> a8.
             ;; White persp: r=0 is rank 8.
             ;; Black persp: r=0 is rank 1.

             (final-c
              (if (eq persp 'black)
                  (- 7 c)
                c))
             (final-r
              (if (eq persp 'black)
                  (- 7 r)
                r))

             (file (+ ?a final-c))
             (rank (- 8 final-r))
             (coord-sym (intern (format "%c%d" file rank))))

        (lichess-game-handle-click coord-sym)))))

(provide 'lichess-game)
;;; lichess-game.el ends here
