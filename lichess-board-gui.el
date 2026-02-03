;;; lichess-board-gui.el --- GUI board renderer -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026  Alexandr Timchenko
;; URL: https://github.com/tmythicator/Lichess.el
;; Version: 0.8
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; SVG-based board renderer.

;;; Code:

(require 'svg)
(require 'lichess-core)

(defcustom lichess-board-gui-asset-path
  (expand-file-name "assets/pieces/cburnett/"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "Path to the directory containing piece SVGs."
  :type 'directory
  :group 'lichess)

(defcustom lichess-board-gui-light-square-color "#f0d9b5"
  "Color of the light squares on the board."
  :type 'color
  :group 'lichess)

(defcustom lichess-board-gui-dark-square-color "#b58863"
  "Color of the dark squares on the board."
  :type 'color
  :group 'lichess)

(defvar lichess-board-gui-themes
  '(("brown" . ("#f0d9b5" . "#b58863"))
    ("blue" . ("#dee3e6" . "#8ca2ad"))
    ("green" . ("#ffffdd" . "#86a666")))
  "Alist of board themes (name . (light . dark)).")

(defun lichess-board-gui-toggle-theme ()
  "Cycle through board themes."
  (interactive)
  (let* ((all-themes (mapcar #'car lichess-board-gui-themes))
         ;; identify current
         ;; For simplicity, just pick next in list or random.
         ;; Or let user choose.
         (choice
          (completing-read "Choose Board Theme: " all-themes nil t)))
    (when-let ((colors (cdr (assoc choice lichess-board-gui-themes))))
      (setq lichess-board-gui-light-square-color (car colors))
      (setq lichess-board-gui-dark-square-color (cdr colors))
      (custom-set-variables
       '(lichess-board-gui-light-square-color (car colors))
       '(lichess-board-gui-dark-square-color (cdr colors)))
      (message "Theme set to %s" choice))))

(defun lichess-board-gui-available-p ()
  "Return t if SVG rendering is available."
  (and (display-graphic-p)
       (fboundp 'image-type-available-p)
       (image-type-available-p 'svg)))

(defun lichess-board-gui--piece-file (piece)
  "Return absolute path to SVG file for character PIECE."
  (let* ((color
          (if (<= ?a piece ?z)
              "b"
            "w"))
         (type (upcase (char-to-string piece)))
         (filename (format "%s%s.svg" color type)))
    (expand-file-name filename lichess-board-gui-asset-path)))

(defun lichess-board-gui--parse-eval (eval-str)
  "Parse EVAL-STR (e.g. \"1.30\", \"#-2\")
into winning probability [0.0, 1.0] for White.
Returns nil if EVAL-STR is invalid, pending, or unavailable."
  (cond
   ((null eval-str)
    nil)
   ((symbolp eval-str)
    nil)
   ((not (stringp eval-str))
    nil)
   ((string= eval-str "")
    nil)
   ((string= eval-str "...")
    nil)
   (t
    (condition-case nil
        (let* ((str (replace-regexp-in-string "+" "" eval-str))
               (is-mate (string-match-p "#" str))
               ;; Verify it looks like a number or mate
               (valid-format (string-match-p "[0-9]" str)))
          (if (not valid-format)
              nil
            (let ((val
                   (string-to-number
                    (replace-regexp-in-string "#" "" str))))
              (if is-mate
                  (if (> val 0)
                      1.0
                    0.0)
                ;; Lichess Sigmoid: 1 / (1 + exp(-0.00368208 * centipawns))
                ;; val is in pawns, so we multiply coeff by 100 -> 0.368208
                (/ 1.0 (+ 1.0 (exp (- (* 0.368208 val)))))))))
      (error
       nil)))))

(defun lichess-board-gui-draw
    (pos &optional perspective highlights eval)
  "Render POS as an SVG image.
PERSPECTIVE: \\='white, \\='black, or \\='auto.
HIGHLIGHTS: List of squares to highlight (e.g. \\='e4).
EVAL: Optional evaluation string (e.g. \"+1.5\")."
  (let* ((sq-size 45) ;; Size of one square in pixels
         (board-size (* sq-size 8))
         (prob (and eval (lichess-board-gui--parse-eval eval)))
         (gauge-width
          (if prob
              12
            0))
         (padding
          (if prob
              6
            0))
         (total-width (+ board-size padding gauge-width))
         (svg (svg-create total-width board-size))
         (persp
          (if (or (null perspective) (eq perspective 'auto))
              (if (eq (plist-get pos :stm) 'b)
                  'black
                'white)
            perspective))
         (flip (eq persp 'black)))

    ;; 1. Draw Board Background (Squares)
    (dotimes (row 8)
      (dotimes (col 8)
        (let* ((is-light (= (% (+ row col) 2) 0))
               (color
                (if is-light
                    lichess-board-gui-light-square-color
                  lichess-board-gui-dark-square-color))
               (x (* col sq-size))
               (y (* row sq-size)))
          (svg-rectangle
           svg
           x
           y
           sq-size
           sq-size
           :fill color
           :stroke-width 0))))

    ;; 2. Draw Highlights
    (dolist (sq highlights)
      (let* ((sq-str (symbol-name sq))
             (file-char (aref sq-str 0))
             (rank-char (aref sq-str 1))
             (col (- file-char ?a))
             (row (- 8 (- rank-char ?0)))
             ;; Flip if needed
             (view-c
              (if flip
                  (- 7 col)
                col))
             (view-r
              (if flip
                  (- 7 row)
                row))
             (x (* view-c sq-size))
             (y (* view-r sq-size)))
        (svg-rectangle
         svg
         x
         y
         sq-size
         sq-size
         :fill "yellow"
         :fill-opacity "0.4"
         :stroke "orange"
         :stroke-width 2)))

    ;; 3. Draw Pieces
    (let ((board (plist-get pos :board)))
      (dotimes (r 8)
        (dotimes (c 8)
          (let* ((piece (aref (aref board r) c))
                 ;; Flip coordinates if black perspective
                 (view-r
                  (if flip
                      (- 7 r)
                    r))
                 (view-c
                  (if flip
                      (- 7 c)
                    c))
                 (x (* view-c sq-size))
                 (y (* view-r sq-size)))

            (unless (eq piece ?.)
              ;; Embed piece SVG (using file path)
              (let ((file (lichess-board-gui--piece-file piece)))
                (when (file-exists-p file)
                  ;; svg-embed modifies the SVG struct, adding an image tag
                  (svg-embed
                   svg
                   file
                   "image/svg+xml"
                   nil
                   :x x
                   :y y
                   :width sq-size
                   :height sq-size))))))))

    ;; 4. Draw Evaluation Gauge
    (when prob
      (let* ((white-h (* board-size prob))
             (white-y (- board-size white-h))
             (gx (+ board-size padding)))

        ;; Draw background (Black/Top)
        (svg-rectangle
         svg
         gx
         0
         gauge-width
         board-size
         :fill "#404040")
        ;; Draw white part
        (svg-rectangle
         svg
         gx
         white-y
         gauge-width
         white-h
         :fill "#e0e0e0")))

    (propertize " " 'display (svg-image svg :ascent 'center))))

(defun lichess-board-gui-missing-assets ()
  "Return a list of missing piece SVG files, or nil if all exist."
  (let ((missing '())
        (pieces '(?K ?Q ?R ?B ?N ?P ?k ?q ?r ?b ?n ?p)))
    (dolist (p pieces)
      (let ((f (lichess-board-gui--piece-file p)))
        (unless (file-exists-p f)
          (push (file-name-nondirectory f) missing))))
    missing))

(defun lichess-board-gui-debug-diagnose ()
  "Print diagnostic info about GUI assets."
  (interactive)
  (message "Asset Path: %s" lichess-board-gui-asset-path)
  (let ((missing (lichess-board-gui-missing-assets)))
    (if missing
        (message "Missing assets: %s" (string-join missing ", "))
      (message "All assets found."))))


(provide 'lichess-board-gui)
;;; lichess-board-gui.el ends here
