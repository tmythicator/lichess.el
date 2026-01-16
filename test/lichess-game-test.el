;;; lichess-game-test.el --- Tests for lichess-game.el logic -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'lichess-game)
(require 'lichess-fen)

(ert-deftest lichess-game-castling-standard-test ()
  "Test standard chess castling move generation."
  (let ((state (make-lichess-game
                :variant "standard"
                :initial-fen "startpos"
                :initial-pos (lichess-fen-parse "startpos"))))
    (with-temp-buffer
      (setq-local lichess-game--state state)
      ;; White
      (should (equal (lichess-game--get-castling-move 'white 'kingside) "e1g1"))
      (should (equal (lichess-game--get-castling-move 'white 'queenside) "e1c1"))
      ;; Black
      (should (equal (lichess-game--get-castling-move 'black 'kingside) "e8g8"))
      (should (equal (lichess-game--get-castling-move 'black 'queenside) "e8c8")))))

(ert-deftest lichess-game-castling-960-test ()
  "Test Chess960 castling (input notation: King captures Rook).
   In Chess960, the 'castling move' is encoded as the King moving onto the Rook's square.
   This disambiguates which side you are castling to."
  
  ;; FEN: 1RK2B1R/pp2pppp/...
  ;; White Position (Rank 1):
  ;; a1=. b1=R c1=K d1=. e1=. f1=B g1=. h1=R
  ;;
  ;; King is at c1.
  ;; Queenside Rook is at b1.
  ;; Kingside Rook is at h1.
  ;;
  ;; Kingside Castle: King (c1) takes Rook (h1) -> "c1h1".
  ;; Queenside Castle: King (c1) takes Rook (b1) -> "c1b1".
  
  (let* ((fen "1RK2B1R/pp2pppp/2n5/2p5/8/8/PPPPPPPP/1RK2B1R w K - 0 1")
         (pos (lichess-fen-parse fen))
         (state (make-lichess-game
                 :variant "chess960"
                 :initial-fen fen
                 :initial-pos pos)))
    (with-temp-buffer
      (setq-local lichess-game--state state)
      ;; Kingside
      (should (equal (lichess-game--get-castling-move 'white 'kingside) "c1h1"))
      ;; Queenside
      (should (equal (lichess-game--get-castling-move 'white 'queenside) "c1b1")))))

(ert-deftest lichess-game-do-castle-logic-test ()
  "Test logic for `lichess-game--do-castle` regarding turn safety."
  ;; We mock `lichess-game-move` to capture the output.
  (let* ((fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") ;; White to move
         (state (make-lichess-game
                 :id "testgame"
                 :variant "standard"
                 :initial-fen fen
                 :initial-pos (lichess-fen-parse fen)
                 :my-color 'white ;; I am White
                 :fen-history (vector fen) ;; History has 1 state
                 ))
         (last-move nil))
    (with-temp-buffer
      (setq-local lichess-game--state state)
      (cl-letf (((symbol-function 'lichess-game-move)
                 (lambda (m) (setq last-move m)))
                ((symbol-function 'message) #'ignore)) ;; Silence prints

        ;; Case 1: I am White, It is White's turn. 'kingside' -> "e1g1"
        (lichess-game--do-castle 'kingside)
        (should (equal last-move "e1g1"))

        ;; Case 2: I am White, It is Black's turn?
        ;; Change FEN stm to 'b'
        (setq last-move nil)
        (aset (lichess-game-fen-history state) 0
              "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1")
        ;; Try to castle. Should NOT call lichess-game-move.
        (lichess-game--do-castle 'kingside)
        (should (null last-move))

        ;; Case 3: I am Black, It is Black's turn.
        (setf (lichess-game-my-color state) 'black)
        (lichess-game--do-castle 'kingside)
        ;; Black Standard Kingside -> "e8g8"
        (should (equal last-move "e8g8"))))))

(ert-deftest lichess-game-full-initialization-test ()
  "Test complete initialization of lichess-game struct from gameFull event."
  (let ((mock-game-full
         '((type . "gameFull")
           (id . "testGameId")
           (initialFen . "startpos")
           (variant . ((key . "standard")))
           (speed . "blitz")
           (perf . ((name . "Blitz")))
           (rated . t)
           (white . ((id . "whitePlayer") (name . "WhitePlayer") (title . "GM")))
           (black . ((id . "blackPlayer") (name . "BlackPlayer")))
           (state . ((moves . "") (wtime . 300000) (btime . 300000) (winc . 0) (binc . 0) (status . "started"))))))
    
    (with-temp-buffer
      (lichess-game-buffer-mode)
      (lichess-game--reset-local-vars)
      
      ;; Simulate finding username to test my-color detection
      (setq-local lichess-username "WhitePlayer")
      
      (lichess-game--board-on-event (current-buffer) mock-game-full)
      
      (let ((st lichess-game--state))
        (should st)
        (should (string= (lichess-game-id st) "testGameId"))
        (should (string= (lichess-game-variant st) "standard"))
        (should (lichess-pos-p (lichess-game-initial-pos st)))
        (should (eq (lichess-game-my-color st) 'white)) 
        
        ;; Check history population
        (let ((hist (lichess-game-fen-history st)))
          (should (> (length hist) 0))
          (should (string-match-p "^rnbqkbnr" (aref hist 0))))
        
        ;; Check verify current-pos parsing
        (should (= (lichess-game-current-idx st) 0))
        (should (lichess-game-live-mode st))))))
