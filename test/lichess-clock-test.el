;;; lichess-clock-test.el --- Tests for clock rendering logic -*- lexical-binding: t; -*-

(require 'ert)
(require 'lichess-game)
(require 'cl-lib)

(ert-deftest lichess-game-format-time-test ()
  "Test formatting milliseconds to MM:SS string."
  (should (equal (lichess-game--format-time 60000) "01:00"))
  (should (equal (lichess-game--format-time 600000) "10:00"))
  (should (equal (lichess-game--format-time 65000) "01:05"))
  (should (equal (lichess-game--format-time 5000) "00:05"))
  (should (equal (lichess-game--format-time 0) "00:00"))
  (should (equal (lichess-game--format-time nil) "--:--")))

(ert-deftest lichess-game-clock-parsing-test ()
  "Test parsing clocks from stream events."
  (let ((mock-event
         '((type . "gameFull")
           (id . "clockTest")
           (initialFen . "startpos")
           (variant . ((key . "standard")))
           (white . ((id . "whitePlayer")))
           (black . ((id . "blackPlayer")))
           (state . ((moves . "") (wtime . 60000) (btime . 120000) (status . "started"))))))
    (with-temp-buffer
      (lichess-game-buffer-mode)
      (lichess-game--reset-local-vars)
      (lichess-game--board-on-event (current-buffer) mock-event)

      (let ((st lichess-game--state))
        (should (equal (lichess-game-white-clock st) "01:00"))
        (should (equal (lichess-game-black-clock st) "02:00"))))))

(ert-deftest lichess-game-spectator-clock-parsing-test ()
  "Test parsing clocks from spectator stream (nested clock object)."
  (let ((mock-event
         '((fen . "startpos")
           (white . ((id . "w")))
           (black . ((id . "b")))
           (clock . ((white . 180000) (black . 300000))))))
    (with-temp-buffer
      (lichess-game-buffer-mode)
      (lichess-game--reset-local-vars)
      ;; Trick to init basic state so update works
      (setf (lichess-game-live-mode lichess-game--state) nil)

      (lichess-game--stream-on-event (current-buffer) mock-event)

      (let ((st lichess-game--state))
        (should (equal (lichess-game-white-clock st) "03:00"))
        (should (equal (lichess-game-black-clock st) "05:00"))))))

(ert-deftest lichess-game-spectator-move-clock-parsing-test ()
  "Test parsing clocks from spectator move event (top-level wc/bc)."
  (let ((mock-event
         '((fen . "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1")
           (wc . 175)
           (bc . 295))))
    (with-temp-buffer
      (lichess-game-buffer-mode)
      (lichess-game--reset-local-vars)
      (lichess-game--stream-on-event (current-buffer) mock-event)

      (let ((st lichess-game--state))
        (should (equal (lichess-game-white-clock st) "02:55"))
        (should (equal (lichess-game-black-clock st) "04:55"))))))

(ert-deftest lichess-game-tick-test ()
  "Test that tick decrements time for the side to move."
  (with-temp-buffer
    (lichess-game-buffer-mode)
    (lichess-game--reset-local-vars)
    ;; Mock float-time to return a fixed value (e.g. 10000.0)
    ;; so calculation is deterministic.
    (cl-letf (((symbol-function 'float-time) (lambda () 10000.0)))
      (let* ((st lichess-game--state)
             (start-ms 10000) ;; 10s
             (fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")) ;; White to move

        (setf (lichess-game-white-time-ms st) start-ms)
        (setf (lichess-game-black-time-ms st) start-ms)
        ;; Fake last update was 1 second ago (9999.0)
        (setf (lichess-game-last-update-time st) 9999.0)

        ;; Setup History so tick can parse FEN
        (setf (lichess-game-fen-history st) (vector fen))
        (setf (lichess-game-current-idx st) 0)
        (setf (lichess-game-live-mode st) t)

        ;; Run Tick. Inside it calls (float-time) -> 10000.0
        ;; Elapsed = 10000.0 - 9999.0 = 1.0s = 1000ms
        (lichess-game--tick (current-buffer))

        ;; White moved? 10s - 1s = 9s. "00:09"
        (should (string= (lichess-game-white-clock st) "00:09"))
        ;; Black should be unchanged "00:10"
        (should (string= (lichess-game-black-clock st) "00:10"))))))
