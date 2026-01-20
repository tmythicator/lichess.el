;;; lichess-game-test.el --- Tests for lichess-game.el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'lichess-game)
(require 'lichess-core)
(require 'cl-lib)

(ert-deftest lichess-game-test-insert-pgn ()
  "Test PGN insertion from game state."
  (let ((state (make-lichess-game
                :id "testgame"
                :moves-str "e2e4 e7e5 g1f3"
                :current-idx 3 ;; Start, 1, 2, 3
                :fen-history (vector "start" "fen1" "fen2" "fen3")))
        (game-buf (get-buffer-create "*test-game*")))
    (with-temp-buffer
      (lichess-game--insert-pgn state game-buf)
      (let ((content (buffer-string)))
        (should (string-search "1. e2e4 e7e5" content))
        (should (string-search "2. g1f3" content))))))

(ert-deftest lichess-game-test-insert-pgn-empty ()
  "Test PGN insertion with no moves."
  (let ((state (make-lichess-game
                :id "testgame-empty"
                :moves-str ""
                :fen-history (vector "start")))
        (game-buf (get-buffer-create "*test-game*")))
    (with-temp-buffer
      (lichess-game--insert-pgn state game-buf)
      (should (string-empty-p (string-trim (buffer-string)))))))

(ert-deftest lichess-game-test-flip-board ()
  "Test logic for flipping board perspective."
  (let ((state (make-lichess-game :perspective 'white)))
    ;; Default white
    (should (eq (lichess-game-perspective state) 'white))
    
    ;; Mocking the render function to avoid GUI calls
    (cl-letf (((symbol-function 'lichess-game-render) #'ignore))
      ;; Flip to black
      (setq lichess-game--state state)
      (lichess-game-flip-board)
      (should (eq (lichess-game-perspective state) 'black))
      
      ;; Flip back to white
      (lichess-game-flip-board)
      (should (eq (lichess-game-perspective state) 'white)))))

(ert-deftest lichess-game-incremental-moves-test ()
  "Test that moves-str is incrementally updated from stream 'lm' events."
  (with-temp-buffer
    (lichess-game-buffer-mode)
    (lichess-game--reset-local-vars)
    (let ((state lichess-game--state)
          (events
           '(
             ;; 1. Initial event (no moves)
             ((id . "incTest") (turns . 0) (moves . ""))
             ;; 2. Move 1
             ((fen . "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1") (lm . "e2e4"))
             ;; 3. Move 2
             ((fen . "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2") (lm . "e7e5")))))
      
      ;; Verify initial state
      (should (string-empty-p (or (lichess-game-moves-str state) "")))

      ;; Process Event 1
      (lichess-game--stream-on-event (current-buffer) (nth 0 events))
      (should (string-empty-p (or (lichess-game-moves-str state) "")))

      ;; Process Event 2 (e2e4)
      (lichess-game--stream-on-event (current-buffer) (nth 1 events))
      (should (equal (lichess-game-moves-str state) "e2e4"))

      ;; Process Event 3 (e7e5)
      (lichess-game--stream-on-event (current-buffer) (nth 2 events))
      (should (equal (lichess-game-moves-str state) "e2e4 e7e5")))))

(ert-deftest lichess-game-render-readonly-test ()
  "Test that rendering works even when buffer is read-only."
  (with-temp-buffer
    (lichess-game-buffer-mode)
    (setq buffer-read-only t) ;; Enforce read-only
    (let ((state (make-lichess-game
                  :id "test"
                  :fen-history (vector "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
                  :current-idx 0
                  :eval-cache (make-hash-table :test 'eql))))
      (setq lichess-game--state state)
      ;; exact behavior: should not error
      (should (progn (lichess-game-render) t))
      ;; Check that buttons were inserted
      (goto-char (point-max))
      (should (search-backward "[Flip Board]" nil t)))))
