;;; .dev.el --- Developer scratchpad for Lichess.el  -*- lexical-binding: t; -*-

;;; Commentary:
;; This file is meant for "Rich Comment" style development.
;; You can evaluate individual S-expressions here (C-x C-e) to control the workflow.
;; It is similar to Clojure's (comment ...) blocks.

;;; Code:

;; 1. Setup
;; Define a Clojure-style comment macro so eval-buffer doesn't run the examples.

(defmacro comment (&rest _body)
  "Ignore BODY.  Used for rich comment blocks."
  nil)

(add-to-list 'load-path default-directory)
(require 'lichess)
(require 'lichess-debug)
(load-file ".secrets.el") ;; Load token
(message "Lichess.el development environment loaded!")

;; 2. Common Entry Points
;; Place cursor after the closing parenthesis and press C-x C-e

;; Start logic
(comment
 (lichess-tv)              ;; Open standard TV
 (lichess-ai-challenge)    ;; Play against Stockfish
 )

;; 3. Live Debugging & State
;; These are useful to eval when you are debugging a specific game.

(comment
 ;; Watch a specific game ID (replace with real ID)
 (defvar lichess-debug-game-ID "t7HAF0vX")
 (lichess-game-watch lichess-debug-game-ID)

 ;; If you have a game buffer open, you can inspect its state.
 (with-current-buffer (get-buffer (format "*Lichess Game Stream: %s*" lichess-debug-game-ID))
   lichess-game--state)

 ;; Check individual properties:
 (with-current-buffer (get-buffer (format "*Lichess Game Stream: %s*" lichess-debug-game-ID))
   (plist-get lichess-game--state :fen-history))

 (with-current-buffer (get-buffer (format "*Lichess Game Stream: %s*" lichess-debug-game-ID))
   (plist-get lichess-game--state :status))

 ;; Helper to inspect arbitrary key
 (defun lichess-dev-get (key)
   (with-current-buffer (get-buffer (format "*Lichess Game Stream: %s*" lichess-debug-game-ID))
     (plist-get lichess-game--state key)))

 (lichess-dev-get :status)

 ;; REPL-style Development (IELM)
 ;; 1. Run M-x ielm
 ;; 2. In IELM, switch context to the game buffer:
 ;;    (ielm-change-working-buffer (get-buffer (format "*Lichess Game Stream: %s*" lichess-debug-game-ID)))
 ;;    ;; Tip: You can also press C-c C-b in IELM to switch working buffer interactively!
 ;; 3. Now you can access locals like `lichess-game--state` directly!

 ;; OR use the new helper (defined below/in lichess-debug.el):
 ;; (lichess-debug-repl lichess-debug-game-ID)
 )

;; 4. Board & Rendering Experiments
(comment
 ;; Preview a FEN string with the TUI/GUI board
 (lichess-fen-show "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

 ;; Test material difference logic
 (lichess-fen-material-diff (lichess-fen-parse "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))

 ;; Test game creation helper
 (lichess-game-create :id "test" :initial-fen "startpos")
 )

;; 5. Mocking & Advanced
(comment
 ;; Create a buffer with a mocked state to test rendering
 (with-current-buffer (get-buffer-create "*Lichess Mock*")
   (lichess-game-buffer-mode)
   (setq-local lichess-game--state
               (lichess-game-create
                :id "mock"
                :white-clock "04:20"
                :black-clock "02:40"
                :fen-history (vector "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
                :current-idx 0))
   (lichess-game-render)
   (pop-to-buffer (current-buffer)))

 ;; Experiment with Position Creation
 (lichess-fen-pos-create :stm 'b :info "Black to move")
 )

;;; .dev.el ends here
