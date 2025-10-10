;;; lichess-fen.el --- FEN parser and Org renderers for Lichess/Emacs -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; - lichess-chess-parse-fen: FEN -> position struct
;; - lichess-fen-render-org-table: Org-mode table (ASCII or Unicode)
;; - lichess-fen-show: interactive preview (Org / Org+Unicode)

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'lichess-core)

(defvar lichess-fen--buf "*Lichess FEN*")

(cl-defstruct lichess-pos
  "Internal chess position. Row 0 = rank 8 (top)."
  board      ;; vector[8] of vector[8] chars
  stm        ;; 'w or 'b
  castle     ;; string like "KQkq" or "-"
  ep         ;; (row . col) or nil
  halfmove   ;; int
  fullmove)  ;; int

;;; FEN -> position
(defun lichess-chess-parse-fen (fen)
  "Parse FEN string into a `lichess-pos' struct."
  (let* ((fields    (split-string (string-trim fen) " +" t))
         (placement (nth 0 fields))
         (active    (nth 1 fields))
         (castle    (or (nth 2 fields) "-"))
         (ep-s      (or (nth 3 fields) "-"))
         (hmc       (string-to-number (or (nth 4 fields) "0")))
         (fmn       (string-to-number (or (nth 5 fields) "1")))
         (rows      (split-string placement "/" t))
         (board     (lichess-fen--rows->board rows))
         (ep        (lichess-fen--parse-ep ep-s)))
    (make-lichess-pos
     :board board
     :stm (if (string= active "b") 'b 'w)
     :castle castle
     :ep ep
     :halfmove hmc
     :fullmove (max 1 fmn))))

(defun lichess-fen--rows->board (rows)
  "Convert 8 FEN ROWS into an 8×8 vector of piece chars.
Each element of ROWS is a string like \"rnbqkbnr\" or \"3p4\".
Return a vector[8] of vector[8] characters. Signals `user-error' on malformed input."
  (unless (= (length rows) 8)
    (user-error "Invalid FEN: expected 8 rows, got %d" (length rows)))
  (let ((board (make-vector 8 nil)))
    (dotimes (r 8)
      (let* ((rowstr (nth r rows))
             (cols   (make-vector 8 ?.))
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
             (user-error "Invalid FEN char: %c at rank %d" ch (- 8 r)))))
         rowstr)
        (when (/= c 8)
          (user-error "FEN row underflow at rank %d" (- 8 r)))
        (aset board r cols)))
    board))

(defun lichess-fen--parse-ep (ep-s)
  "Parse en passant field like \"e3\" from FEN.
Return (row . col) or nil. Valid ranks are only 3 and 6."
  (when (and (stringp ep-s) (not (string= ep-s "-")))
    (unless (string-match-p "\\`[a-hA-H][36]\\'" ep-s)
      (user-error "Invalid en passant square: %S" ep-s))
    (let* ((file (downcase (aref ep-s 0)))
           (rank (string-to-number (string (aref ep-s 1)))) ;; e.g. 3 or 6
           (col  (cl-position file "abcdefgh"))        ;; a->0, ... h->7
           (row  (- 8 rank)))
      (cons row col))))

;;; Org renderers
(defun lichess-fen--piece->unicode (ch)
  "Map ASCII piece CH to Unicode figure (string)."
  (pcase ch
    (?K "♔") (?Q "♕") (?R "♖") (?B "♗") (?N "♘") (?P "♙")
    (?k "♚") (?q "♛") (?r "♜") (?b "♝") (?n "♞") (?p "♟")
    (_ ".")))

(defun lichess-fen-render-org-table (pos &optional unicode perspective)
  "Return an Org-mode table string for POS.
If UNICODE non-nil, use Unicode chess glyphs.
PERSPECTIVE is 'white or 'black. If nil, derive from (lichess-pos-stm pos)."
  (let* ((b     (lichess-pos-board pos))
         (fmt   (if unicode
                    #'lichess-fen--piece->unicode
                  (lambda (ch) (if (= ch ?.) "." (char-to-string ch)))))
         (persp   (or perspective
                      (if (eq (lichess-pos-stm pos) 'b) 'black 'white)))
         (flip  (eq persp 'black))
         (col-seq (if flip (number-sequence 7 0 -1) (number-sequence 0 7)))
         (row-seq (if flip (number-sequence 7 0 -1) (number-sequence 0 7)))
         (files   (if flip '("h" "g" "f" "e" "d" "c" "b" "a")
                    '("a" "b" "c" "d" "e" "f" "g" "h")))
         (header (format "|   | %s |" (mapconcat #'identity files " | ")))
         (sep "|-+-+---+---+---+---+---+---+---+---|"))
    (cl-labels
        ((row->line
           (r)
           (let* ((rowv (aref b r)) ;; current row vector
                  (cells (mapcar (lambda (c) (funcall fmt (aref rowv c))) col-seq))
                  (rank-label (- 8 r)))
             (format "| %d | %s |" rank-label (string-join cells " | ")))))
      (string-join (append (list header sep) (mapcar #'row->line row-seq)) "\n"))))

(defun lichess-fen-render-heading (pos style)
  "Return heading string for the POS with chosen STYLE."
  (format "FEN preview (%s), side-to-move: %s, castle: %s, ep: %s, hm: %d, fm: %d\n\n"
          style (if (eq (lichess-pos-stm pos) 'w) "white" "black")
          (lichess-pos-castle pos)
          (or (when-let ((ep (lichess-pos-ep pos)))
                (format "%c%d" (+ ?a (cdr ep)) (- 8 (car ep))))
              "-")
          (lichess-pos-halfmove pos)
          (lichess-pos-fullmove pos)))

;;;###autoload
(defun lichess-fen-show (fen style &optional perspective)
  "Render FEN in a preview buffer using STYLE and PERSPECTIVE.
STYLE: \"org\" or \"org+unicode\".
PERSPECTIVE: 'white, 'black, or 'from-stm (default 'from-stm)."

  (interactive
   (list (read-string "FEN: ")
         (completing-read "Style: " '("org" "org+unicode") nil t "org+unicode")
         (let* ((choices '("from-stm" "white" "black")))
           (completing-read "Perspective: " choices nil t "from-stm"))))
  (let* ((pos (lichess-chess-parse-fen fen))
         (buf (get-buffer-create lichess-fen--buf))
         (unicode (string= style "org+unicode"))
         (persp-raw (cond
                     ((symbolp perspective) perspective)
                     ((stringp perspective) (intern perspective))
                     (t 'from-stm)))
         (persp (if (eq persp-raw 'from-stm)
                    (if (eq (lichess-pos-stm pos) 'b) 'black 'white)
                  persp-raw)))
    (with-current-buffer buf (lichess-core-mode))
    (lichess-core-with-buf buf
      (erase-buffer)
      (insert (lichess-fen-render-heading pos style))
      (insert (lichess-fen-render-org-table pos unicode persp))
      (insert "\n"))
    (pop-to-buffer buf)
    (when (and (require 'org-table nil t))
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (org-table-align))))))

(provide 'lichess-fen)
;;; lichess-fen.el ends here