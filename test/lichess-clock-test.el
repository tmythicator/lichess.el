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
        (should (equal (plist-get st :white-clock) "01:00"))
        (should (equal (plist-get st :black-clock) "02:00"))))))

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
      (plist-put lichess-game--state :live-mode nil)

      (lichess-game--stream-on-event (current-buffer) mock-event)

      (let ((st lichess-game--state))
        (should (equal (plist-get st :white-clock) "03:00"))
        (should (equal (plist-get st :black-clock) "05:00"))))))

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
        (should (equal (plist-get st :white-clock) "02:55"))
        (should (equal (plist-get st :black-clock) "04:55"))))))

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

        (plist-put st :white-time-ms start-ms)
        (plist-put st :black-time-ms start-ms)
        ;; Fake last update was 1 second ago (9999.0)
        (plist-put st :last-update-time 9999.0)

        ;; Setup History so tick can parse FEN
        (plist-put st :fen-history (vector fen))
        (plist-put st :current-idx 0)
        (plist-put st :live-mode t)

        ;; Run Tick. Inside it calls (float-time) -> 10000.0
        ;; Elapsed = 10000.0 - 9999.0 = 1.0s = 1000ms
        (let ((inhibit-read-only t))
          (lichess-game--tick (current-buffer)))

        ;; White moved? 10s - 1s = 9s. "00:09"
        (should (string= (plist-get st :white-clock) "00:09"))
        ;; Black should be unchanged "00:10"
        (should (string= (plist-get st :black-clock) "00:10"))))))

(ert-deftest lichess-game-tick-in-place-test ()
  "Test that tick updates the clock string in-place."
  (with-temp-buffer
    (lichess-game-buffer-mode)
    (lichess-game--reset-local-vars)
    (cl-letf (((symbol-function 'float-time) (lambda () 10000.0)))
      (let* ((st lichess-game--state)
             (fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))

        ;; Setup initial state
        (plist-put st :white-time-ms 10000)
        (plist-put st :black-time-ms 10000)
        (plist-put st :white-clock "00:10")
        (plist-put st :black-clock "00:10")
        (plist-put st :last-update-time 9999.0)
        (plist-put st :fen-history (vector fen))
        (plist-put st :current-idx 0)
        (plist-put st :live-mode t)

        ;; Manually insert text with expected properties (simulating render)
        (let ((inhibit-read-only t))
          (insert "White (")
          (insert (propertize "00:10" 'lichess-clock 'white))
          (insert ") vs Black (")
          (insert (propertize "00:10" 'lichess-clock 'black))
          (insert ")\n"))

        ;; Run Tick -> Should update White to 00:09 IN PLACE
        (lichess-game--tick (current-buffer))

        (goto-char (point-min))
        ;; Verify content changed
        (should (search-forward "White (00:09)" nil t))

        ;; Verify in-place update worked
        (goto-char (point-min))
        (let ((match (text-property-search-forward 'lichess-clock 'white t)))
          (should match)
          (should (string= (buffer-substring-no-properties (prop-match-beginning match) (prop-match-end match)) "00:09")))))))

(ert-deftest lichess-game-termination-test ()
  "Test game termination handling (status/winner)."
  (with-temp-buffer
    (lichess-game-buffer-mode)
    (lichess-game--reset-local-vars)
    (let* ((st lichess-game--state)
           (event '((type . "gameFull")
                    (id . "termTest")
                    (status . "started")
                    )))
      ;; Set initial state with valid history/index so render works
      (plist-put st :live-mode t)
      (plist-put st :fen-history (vector "startpos"))
      (plist-put st :current-idx 0)

      ;; 1. Simulate Stream Event with termination
      (let ((term-event '((status . "resign") (winner . "white"))))
        (lichess-game--stream-on-event (current-buffer) term-event)

        (should (string= (plist-get st :status) "resign"))
        (should (eq (plist-get st :winner) 'white))
        (should (null (plist-get st :live-mode)))
        ;; Verify render happened (result string present)
        (goto-char (point-min))
        (should (search-forward "White is victorious • 1-0 (resign)" nil t)))

      ;; 2. Simulate Board API Event with termination
      ;; Reset
      (plist-put st :live-mode t)
      (let ((term-event-board '((type . "gameState") (status . "mate") (winner . "black"))))
        (lichess-game--board-on-event (current-buffer) term-event-board)

        (should (string= (plist-get st :status) "mate"))
        (should (eq (plist-get st :winner) 'black))
        (should (null (plist-get st :live-mode)))))))

(ert-deftest lichess-game-status-object-test ()
  "Test that status update handles objects (e.g. {id: 35, name: 'outoftime'})."
  (let ((state (lichess-game-create))
        (obj '((status . ((id . 35) (name . "outoftime")))
               (winner . "white"))))
    (lichess-game--update-status state obj (current-buffer))
    (should (string= (plist-get state :status) "outoftime"))
    (should (eq (plist-get state :winner) 'white))))
