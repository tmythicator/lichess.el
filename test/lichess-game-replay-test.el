;;; lichess-game-replay-test.el --- Test for NDJSON history replay logic -*- lexical-binding: t; -*-

(require 'ert)
(require 'lichess-game)

(ert-deftest lichess-game-replay-test ()
  "Test that the history is correctly reset when a stream sends summary then replays."
  (with-temp-buffer
    (lichess-game-buffer-mode)
    (lichess-game--reset-local-vars)
    (let ((state lichess-game--state)
          (events
           '(
             ;; 1. Summary Event (Latest state, turn 71)
             ((id . "t7HAF0vX")
              (variant (key . "standard"))
              (fen . "7R/2pkn2p/1p4pP/3Pp1P1/4P3/1B6/1P6/2KR4 b - - 0 36")
              (turns . 71))

             ;; 2. Startpos (Replay begins)
             ((fen . "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))

             ;; 3. Move 1
             ((fen . "rnbqkbnr/pppppppp/8/8/2P5/8/PP1PPPPP/RNBQKBNR b KQkq - 0 1") (lm . "c2c4"))
             
             ;; ... Simplified sequence for test ...
             ;; 4. Move 2
             ((fen . "rnbqkbnr/pppppp1p/6p1/8/2P5/8/PP1PPPPP/RNBQKBNR w KQkq - 0 2") (lm . "g7g6"))
             )))

      ;; Process events
      (dolist (evt events)
        (lichess-game--stream-on-event (current-buffer) evt))

      ;; Verification
      (let ((hist (plist-get state :fen-history)))
        ;; Logic currently fails here: it appends everything.
        ;; Expected:
        ;; - If we detect restart, index 0 should be startpos.
        ;; - Length should correspond to the replay sequence, not Summary + Replay.
        
        ;; In this simplified test:
        ;; Event 1: Pushes Summary FEN (len=1)
        ;; Event 2: Startpos. SHOULD reset history. (len=1)
        ;; Event 3: Move 1. (len=2)
        ;; Event 4: Move 2. (len=3)
        
        ;; Currently (Buggy):
        ;; Event 1: Summary (len=1)
        ;; Event 2: Startpos (len=2) -> [Summary, Startpos]
        ;; Event 3: Move 1 (len=3)
        ;; ...
        
        ;; We assert the behavior we WANT:
        (should (equal (aref hist 0) "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))
        (should (= (length hist) 3)) ;; Start, Move 1, Move 2
        (should (equal (aref hist 2) "rnbqkbnr/pppppp1p/6p1/8/2P5/8/PP1PPPPP/RNBQKBNR w KQkq - 0 2"))))))

(provide 'lichess-game-replay-test)
