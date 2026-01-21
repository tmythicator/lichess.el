;;; lichess-board-eval-test.el --- Tests for eval parsing  -*- lexical-binding: t; -*-

(require 'ert)
(require 'lichess-board-gui)

(ert-deftest lichess-board-eval-parse-test ()
  "Test parsing of evaluation strings."
  
  ;; 1. Empty / Invalid / Symbol -> nil (no render)
  (should (null (lichess-board-gui--parse-eval nil)))
  (should (null (lichess-board-gui--parse-eval :pending)))
  (should (null (lichess-board-gui--parse-eval :unavailable)))
  (should (null (lichess-board-gui--parse-eval "")))
  (should (null (lichess-board-gui--parse-eval "...")))
  (should (null (lichess-board-gui--parse-eval "invalid")))

  ;; 2. Centipawns (Lichess Sigmoid)
  ;; 0.0 -> 0.5
  (should (= (lichess-board-gui--parse-eval "0.00") 0.5))
  ;; +1.0 (100cp) -> ~0.59
  (let ((val (lichess-board-gui--parse-eval "1.00")))
    (should (> val 0.58))
    (should (< val 0.60)))
  ;; +5.0 (500cp) -> ~0.86
  (let ((val (lichess-board-gui--parse-eval "5.00")))
    (should (> val 0.85))
    (should (< val 0.87)))
  ;; -2.5 (-250cp) -> ~0.28
  (let ((val (lichess-board-gui--parse-eval "-2.50")))
    (should (> val 0.27))
    (should (< val 0.29)))
  
  ;; 3. Mate
  ;; Mate positive -> 1.0
  (should (= (lichess-board-gui--parse-eval "#3") 1.0))
  ;; Mate negative -> 0.0
  (should (= (lichess-board-gui--parse-eval "#-2") 0.0)))

(provide 'lichess-board-eval-test)
