;;; lichess-board-gui.el --- GUI board renderer -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026  Alexandr Timchenko
;; URL: https://github.com/tmythicator/Lichess.el
;; Version: 0.3
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

(defun lichess-board-gui-draw (pos &optional perspective highlights)
  "Render POS as an SVG image.
PERSPECTIVE: \\='white, \\='black, or \\='auto.
HIGHLIGHTS: List of squares to highlight (e.g. \\='e4)."
  (let* ((sq-size 45) ;; Size of one square in pixels
         (board-size (* sq-size 8))
         (svg (svg-create board-size board-size))
         (persp
          (if (or (null perspective) (eq perspective 'auto))
              (if (eq (lichess-pos-stm pos) 'b)
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
                    "#f0d9b5"
                  "#b58863")) ;; Standard wood theme
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
    (let ((board (lichess-pos-board pos)))
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
