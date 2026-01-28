;;; lichess-fen.el --- FEN parser and board renderers for Lichess -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026  Alexandr Timchenko
;; URL: https://github.com/tmythicator/Lichess.el
;; Version: 0.7
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; - lichess-fen-parse: FEN -> position struct
;; - lichess-fen-render-board: core board renderer
;; - lichess-fen-show: interactive preview (ASCII / Unicode)

;;; Code:

(require 'seq)
(require 'cl-lib)
(require 'subr-x)
(require 'lichess-core)
(require 'lichess-board)


(defvar lichess-fen--buf "*Lichess FEN Preview*")

;;; FEN -> position
(defun lichess-fen-parse (fen)
  "Parse FEN string into a `lichess-pos' struct."
  (let*
      ((raw-fen
        (if (string= fen "startpos")
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
          fen))
       (fields (split-string (string-trim raw-fen) " +" t))
       (placement (nth 0 fields))
       (active (nth 1 fields))
       (castle (or (nth 2 fields) "-"))
       (ep-s (or (nth 3 fields) "-"))
       (hmc (string-to-number (or (nth 4 fields) "0")))
       (fmn (string-to-number (or (nth 5 fields) "1")))
       (rows (split-string placement "/" t))
       (board (lichess-fen--rows->board rows))
       (ep (lichess-fen--parse-ep ep-s)))
    (make-lichess-pos
     :board board
     :stm
     (if (string= active "b")
         'b
       'w)
     :castle castle
     :ep ep
     :halfmove hmc
     :fullmove (max 1 fmn))))

(defun lichess-fen--rows->board (rows)
  "Convert 8 FEN ROWS into an 8×8 vector of piece chars.
Each element of ROWS is a string like \"rnbqkbnr\" or \"3p4\".
Return a vector[8] of vector[8] characters.
Signals \`user-error' on malformed input."
  (unless (= (length rows) 8)
    (user-error "Invalid FEN: expected 8 rows, got %d" (length rows)))
  (let ((board (make-vector 8 nil)))
    (dotimes (r 8)
      (let* ((rowstr (nth r rows))
             (cols (make-vector 8 ?.))
             (c 0))
        ;; iterate over each char
        (seq-do
         (lambda (ch)
           (cond
            ;; if number n -> skip n cells
            ((and (>= ch ?1) (<= ch ?8))
             (setq c (+ c (string-to-number (char-to-string ch)))))
            ;; pieces -> set in vector
            ((memq ch '(?p ?r ?n ?b ?q ?k ?P ?R ?N ?B ?Q ?K))
             (when (> c 7)
               (user-error "FEN row overflow at rank %d" (- 8 r)))
             (aset cols c ch)
             (setq c (1+ c)))
            (t
             (user-error "Invalid FEN char: %c at rank %d"
                         ch
                         (- 8 r)))))
         rowstr)
        (when (/= c 8)
          (user-error "FEN row underflow at rank %d" (- 8 r)))
        (aset board r cols)))
    board))

(defun lichess-fen--parse-ep (ep-s)
  "Parse en passant field like \"e3\" from FEN.
Return (row . col) or nil.  Valid ranks are only 3 and 6.
EP-S is the en passant target string."
  (when (and (stringp ep-s) (not (string= ep-s "-")))
    (unless (string-match-p "\\`[a-hA-H][36]\\'" ep-s)
      (user-error "Invalid en passant square: %S" ep-s))
    (let*
        ((file (downcase (aref ep-s 0)))
         (rank (string-to-number (string (aref ep-s 1)))) ;; e.g. 3 or 6
         (col (cl-position file "abcdefgh")) ;; a->0, ... h->7
         (row (- 8 rank)))
      (cons row col))))

(defvar-local lichess-fen--current-pos nil
  "The current `lichess-pos` being displayed.")
(defvar-local lichess-fen--current-persp nil
  "The current perspective.")

(defun lichess-fen-refresh ()
  "Redraw the current FEN buffer using the latest style settings."
  (when (and lichess-fen--current-pos lichess-fen--current-persp)
    (lichess-board-render-to-buffer
     lichess-fen--current-pos lichess-fen--current-persp)
    (let ((style
           (if (and (display-graphic-p)
                    (lichess-board-gui-available-p))
               lichess-board-gui-preferred-style
             lichess-board-tui-preferred-style)))
      (message "Lichess FEN shown (Style: %s)" style))))

;;;###autoload
(defun lichess-fen-show (fen &optional perspective)
  "Render FEN in a preview buffer using the preferred style and PERSPECTIVE.
PERSPECTIVE: `white', `black', or `from-stm' (default `from-stm')."

  (interactive (list
                (read-string "FEN: " nil nil "startpos")
                (let* ((choices '("from-stm" "white" "black")))
                  (completing-read "Perspective: " choices
                                   nil
                                   t
                                   "from-stm"))))
  (let* ((pos (lichess-fen-parse fen))
         (buf (get-buffer-create lichess-fen--buf))
         (persp-raw
          (cond
           ((or (null perspective) (eq perspective 'from-stm))
            'from-stm)
           ((symbolp perspective)
            perspective)
           ((stringp perspective)
            (intern perspective))
           (t
            'from-stm)))
         (persp
          (if (eq persp-raw 'from-stm)
              (if (eq (lichess-pos-stm pos) 'b)
                  'black
                'white)
            persp-raw)))
    (pop-to-buffer buf)
    (lichess-core-mode)
    (setq
     lichess-fen--current-pos pos
     lichess-fen--current-persp persp)
    (lichess-fen-refresh)))

;;; Move application
(defun lichess-fen-apply-moves (pos moves-str)
  "Return a new `lichess-pos' by applying UCI MOVES-STR to POS.
MOVES-STR is a space-separated string of UCI moves like \"e2e4 e7e5\"."
  (let ((new-pos (copy-lichess-pos pos))
        (moves (split-string (or moves-str "") " " t)))
    ;; Deep copy the board vector of vectors
    (let ((old-board (lichess-pos-board new-pos))
          (new-board (make-vector 8 nil)))
      (dotimes (i 8)
        (aset new-board i (copy-sequence (aref old-board i))))
      (setf (lichess-pos-board new-pos) new-board))

    (dolist (m moves)
      (lichess-fen--apply-uci-move new-pos m))
    new-pos))

(defun lichess-fen--apply-uci-move (pos uci)
  "Apply a single UCI move string to POS (modifies POS in place)."
  (when (>= (length uci) 4)
    (let* ((f-col (- (aref uci 0) ?a))
           (f-row (- 8 (- (aref uci 1) ?0)))
           (t-col (- (aref uci 2) ?a))
           (t-row (- 8 (- (aref uci 3) ?0)))
           (board (lichess-pos-board pos))
           (piece (aref (aref board f-row) f-col))
           (is-white (eq (lichess-pos-stm pos) 'w)))

      (when piece
        ;; 1. Handle Castling
        (when (and (memq piece '(?K ?k)) (= (abs (- f-col t-col)) 2))
          (let ((rank-row
                 (if is-white
                     7
                   0)))
            (cond
             ((= t-col 6) ;; Kingside
              (let ((rook (aref (aref board rank-row) 7)))
                (aset (aref board rank-row) 5 rook)
                (aset (aref board rank-row) 7 ?.)))
             ((= t-col 2) ;; Queenside
              (let ((rook (aref (aref board rank-row) 0)))
                (aset (aref board rank-row) 3 rook)
                (aset (aref board rank-row) 0 ?.))))))

        ;; 2. Handle En Passant
        (when (and (memq piece '(?P ?p))
                   (/= f-col t-col)
                   (eq (aref (aref board t-row) t-col) ?.))
          (aset (aref board f-row) t-col ?.))

        ;; 3. Handle Promotion
        (when (= (length uci) 5)
          (let ((pchar (aref uci 4)))
            (setq piece
                  (if is-white
                      (upcase pchar)
                    (downcase pchar)))))

        ;; 4. Move Piece
        (aset (aref board t-row) t-col piece)
        (aset (aref board f-row) f-col ?.)

        ;; 5. Update side to move
        (setf (lichess-pos-stm pos)
              (if is-white
                  'b
                'w))

        (unless is-white
          (setf (lichess-pos-fullmove pos)
                (1+ (lichess-pos-fullmove pos))))))))

(defun lichess-fen-pos->fen (pos)
  "Convert POS struct back into a FEN string."
  (let* ((board (lichess-pos-board pos))
         (rows '()))
    (dotimes (r 8)
      (let ((rowv (aref board r))
            (rowstr "")
            (empty 0))
        (dotimes (c 8)
          (let ((p (aref rowv c)))
            (if (eq p ?.)
                (cl-incf empty)
              (progn
                (when (> empty 0)
                  (setq rowstr
                        (concat rowstr (number-to-string empty)))
                  (setq empty 0))
                (setq rowstr (concat rowstr (string p)))))))
        (when (> empty 0)
          (setq rowstr (concat rowstr (number-to-string empty))))
        (push rowstr rows)))
    (concat
     (string-join (reverse rows) "/")
     " "
     (if (eq (lichess-pos-stm pos) 'w)
         "w"
       "b")
     " "
     (or (lichess-pos-castle pos) "-")
     " "
     "-" ;; simplified ep
     " "
     (number-to-string (lichess-pos-halfmove pos))
     " "
     (number-to-string (lichess-pos-fullmove pos)))))

(defun lichess-fen-material-diff (pos)
  "Calculate material difference for POS.
Returns a list (w-score b-score w-diff b-diff).
w-score/b-score: Total material value (P=1, N/B=3, R=5, Q=9).
w-diff/b-diff: List of characters representing extra pieces for that side."
  (let ((board (lichess-pos-board pos))
        (w-counts
         (list
          (cons ?P 0)
          (cons ?N 0)
          (cons ?B 0)
          (cons ?R 0)
          (cons ?Q 0)))
        (b-counts
         (list
          (cons ?p 0)
          (cons ?n 0)
          (cons ?b 0)
          (cons ?r 0)
          (cons ?q 0))))
    ;; 1. Count pieces
    (dotimes (r 8)
      (dotimes (c 8)
        (let ((p (aref (aref board r) c)))
          (cond
           ((assoc p w-counts)
            (cl-incf (cdr (assoc p w-counts))))
           ((assoc p b-counts)
            (cl-incf (cdr (assoc p b-counts))))))))

    ;; 2. Calculate values
    (let ((w-points 0)
          (b-points 0)
          (w-extra '())
          (b-extra '())
          (values '((?P . 1) (?N . 3) (?B . 3) (?R . 5) (?Q . 9))))

      (dolist (pair values)
        (let* ((char (car pair))
               (val (cdr pair))
               (w-count (cdr (assoc char w-counts)))
               (b-char (downcase char))
               (b-count (cdr (assoc b-char b-counts)))
               (diff (- w-count b-count)))

          (cl-incf w-points (* w-count val))
          (cl-incf b-points (* b-count val))

          (cond
           ((> diff 0)
            (dotimes (_ diff)
              (push char w-extra)))
           ((< diff 0)
            (dotimes (_ (abs diff))
              (push b-char b-extra))))))

      (list
       w-points b-points (nreverse w-extra) (nreverse b-extra)))))

(defun lichess-fen-format-material (diff)
  "Format material DIFF from `lichess-fen-material-diff`.
Returns (w-string . b-string)."
  (let* ((w-pts (nth 0 diff))
         (b-pts (nth 1 diff))
         (w-extra (nth 2 diff))
         (b-extra (nth 3 diff))
         (pt-diff (- w-pts b-pts))
         (map-piece
          (lambda (chars)
            (mapconcat (lambda (c)
                         (pcase (upcase c)
                           (?P "♙")
                           (?N "♘")
                           (?B "♗")
                           (?R "♖")
                           (?Q "♕")
                           (_ (char-to-string c))))
                       chars
                       ""))))
    (cons
     (if (> pt-diff 0)
         (format " +%d %s" pt-diff (funcall map-piece w-extra))
       (if w-extra
           (concat " " (funcall map-piece w-extra))
         ""))
     (if (< pt-diff 0)
         (format " +%d %s" (abs pt-diff) (funcall map-piece b-extra))
       (if b-extra
           (concat " " (funcall map-piece b-extra))
         "")))))

(provide 'lichess-fen)
;;; lichess-fen.el ends here
