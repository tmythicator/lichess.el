;;; lichess-board-eval-test.el --- Tests for eval parsing  -*- lexical-binding: t; -*-

(require 'ert)
(require 'lichess-board-gui)

(ert-deftest lichess-board-eval-parse-test ()
  "Test parsing of evaluation strings."
  
  ;; 1. Empty / Invalid -> 0.5
  (should (= (lichess-board-gui--parse-eval nil) 0.5))
  (should (= (lichess-board-gui--parse-eval "") 0.5))
  (should (= (lichess-board-gui--parse-eval "...") 0.5))
  (should (= (lichess-board-gui--parse-eval "invalid") 0.5))

  ;; 2. Centipawns (Sigmoid check)
  ;; 0.0 -> 0.5
  (should (= (lichess-board-gui--parse-eval "0.0") 0.5))
  ;; +1.0 -> > 0.5
  (should (> (lichess-board-gui--parse-eval "1.0") 0.5))
  ;; -1.0 -> < 0.5
  (should (< (lichess-board-gui--parse-eval "-1.0") 0.5))
  
  ;; 3. Mate
  ;; Mate positive -> 1.0
  (should (= (lichess-board-gui--parse-eval "#3") 1.0))
  ;; Mate negative -> 0.0
  (should (= (lichess-board-gui--parse-eval "#-2") 0.0)))

(provide 'lichess-board-eval-test)
