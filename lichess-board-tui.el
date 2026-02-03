;;; lichess-board-tui.el --- Text-based board renderer -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026  Alexandr Timchenko
;; URL: https://github.com/tmythicator/Lichess.el
;; Version: 0.8
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; TUI (Text User Interface) renderer for Lichess boards.
;; Uses ASCII or Unicode characters.

;;; Code:

(require 'seq)
(require 'cl-lib)
(require 'subr-x)
(require 'lichess-core)

(defun lichess-board-tui--piece->unicode (ch)
  "Map ASCII piece CH to Unicode figure (string)."
  (pcase ch
    (?K "♔")
    (?Q "♕")
    (?R "♖")
    (?B "♗")
    (?N "♘")
    (?P "♙")
    (?k "♚")
    (?q "♛")
    (?r "♜")
    (?b "♝")
    (?n "♞")
    (?p "♟")
    (_ "·")))

(defun lichess-board-tui--render-evaluation-bar
    (eval-str height perspective)
  "Return a list of strings representing a vertical evaluation bar.
EVAL-STR is the evaluation.  HEIGHT is the bar height.
PERSPECTIVE determines the view."
  (let* ((bar (make-vector height " "))
         (advantage-char "█")
         (disadvantage-char "░")
         (max-eval 8.0)
         (eval-num
          (cond
           ((null eval-str)
            0.0)
           ((string-prefix-p "M" eval-str)
            (if (string-prefix-p "M-" eval-str)
                (- max-eval)
              max-eval))
           (t
            (string-to-number eval-str))))
         (relative-eval
          (if (eq perspective 'black)
              (- eval-num)
            eval-num)))
    (setq relative-eval
          (max (- max-eval) (min max-eval relative-eval)))
    (let* ((mid-point (/ (1- height) 2.0))
           (fill-ratio (/ relative-eval max-eval))
           (advantage-blocks (round (* (+ 1 fill-ratio) mid-point))))
      (dotimes (i height)
        (if (< i (- height advantage-blocks))
            (aset bar i disadvantage-char)
          (aset bar i advantage-char))))
    (append '("Eval") (append bar nil))))

(defun lichess-board-tui-draw (pos &optional style perspective)
  "Return a board string for POS.
STYLE is \"ascii\" or \"unicode\".
PERSPECTIVE control rendering style.
Reads `eval` from POS if present."
  (let* ((unicode (string= style "unicode"))
         (eval-str (plist-get pos :eval))
         (board-lines
          ;; First, generate the board as a list of strings
          (let* ((b (plist-get pos :board))
                 (fmt
                  (if unicode
                      #'lichess-board-tui--piece->unicode
                    (lambda (ch)
                      (if (= ch ?.)
                          "."
                        (char-to-string ch)))))
                 (persp
                  (if (or (null perspective) (eq perspective 'auto))
                      (if (eq (plist-get pos :stm) 'b)
                          'black
                        'white)
                    perspective))
                 (flip (eq persp 'black))
                 (col-seq
                  (if flip
                      (number-sequence 7 0 -1)
                    (number-sequence 0 7)))
                 (row-seq
                  (if flip
                      (number-sequence 7 0 -1)
                    (number-sequence 0 7)))
                 (files
                  (if flip
                      '("h" "g" "f" "e" "d" "c" "b" "a")
                    '("a" "b" "c" "d" "e" "f" "g" "h")))
                 (header
                  (format "|%s| " (mapconcat #'identity files "|")))
                 (sep "|-+-+-+-+-+-+-+-+-"))
            (cl-labels
             ((row->line
               (r)
               (let* ((rowv (aref b r))
                      (cells
                       (mapcar
                        (lambda (c)
                          (let* ((ch (aref rowv c))
                                 (str (funcall fmt ch)))
                            str))
                        col-seq))
                      (rank-label (- 8 r)))
                 (format "|%s|%d"
                         (string-join cells "|")
                         rank-label))))
             (append
              (mapcar #'row->line row-seq) (list sep header)))))

         (board-rows (seq-subseq board-lines 0 8))
         (sep (nth 8 board-lines))
         (header (nth 9 board-lines)))

    (if (or (not eval-str) (string= eval-str "..."))
        ;; If no eval-str, just return the board as a single string.
        (string-join (append board-rows (list sep header)) "\n")

      ;; If eval-str exists, stitch the bar to the right of the board.
      (let* ((bar-lines
              (lichess-board-tui--render-evaluation-bar
               eval-str 8 perspective))
             (bar-header (car bar-lines))
             (bar-body (cdr bar-lines)) ; The 8 characters of the bar
             (stitched-rows '()))
        ;; Combine each board row with its corresponding bar character.
        (dotimes (i 8)
          (push (format "%s  %s" (nth i board-rows) (nth i bar-body))
                stitched-rows))

        (string-join (append
                      (reverse stitched-rows)
                      (list sep (format "%s  %s" header bar-header)))
                     "\n")))))

(defun lichess-board-tui-draw-heading (pos style perspective)
  "Return a heading string for POS using STYLE and PERSPECTIVE.
STYLE is \"ascii\" or \"unicode\"."
  (let* ((persp
          (if (or (null perspective) (eq perspective 'auto))
              (if (eq (plist-get pos :stm) 'b)
                  'black
                'white)
            perspective))
         (stm
          (if (eq (plist-get pos :stm) 'w)
              "White"
            "Black"))
         (castle (or (plist-get pos :castle) "-"))
         (ep (or (plist-get pos :ep) "-"))
         (full (plist-get pos :fullmove))
         (tag
          (cond
           ((string= style "unicode")
            "Unicode")
           ((string= style "ascii")
            "ASCII")
           (t
            style))))
    (format "* %s moves (Castle: %s, EP: %s, Full: %d) [%s, %s]\n"
            stm
            castle
            ep
            full
            persp
            tag)))

(provide 'lichess-board-tui)
;;; lichess-board-tui.el ends here
