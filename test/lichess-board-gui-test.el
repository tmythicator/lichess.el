;;; lichess-board-gui-test.el --- Tests for lichess-board-gui.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'lichess-board-gui)
(require 'lichess-fen)

(ert-deftest lichess-board-gui-color-customization-test ()
  "Test that board rendering respects custom color variables."
  (let ((lichess-board-gui-light-square-color "#aabbcc")
        (lichess-board-gui-dark-square-color "#112233")
        ;; Create a minimal dummy position (empty board)
        (pos (lichess-fen-parse "8/8/8/8/8/8/8/8 w - - 0 1")))
    
    (let* ((result (lichess-board-gui-draw pos))
           (display-prop (get-text-property 0 'display result))
           (image-data (plist-get (cdr display-prop) :data)))
      
      ;; Verify colors are present in the SVG data
      (should (string-match-p "#aabbcc" image-data))
      (should (string-match-p "#112233" image-data)))))
