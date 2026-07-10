;;; lichess-ai-test.el --- Tests for lichess-ai.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; Add project root to load-path
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory (or load-file-name (buffer-file-name))))))

(require 'lichess-ai)

;;; Code:

(ert-deftest lichess-ai-start-game-success-test ()
  "Test that `lichess-ai--start-game` calls API and triggers game play on success."
  (let ((api-called nil)
        (play-called nil)
        (messages '())
        (res-mock (lichess-http-result-ok '((id . "game123")))))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages)))
              ((symbol-function 'lichess-api-challenge-ai)
               (lambda (level color limit increment fen callback text-mode)
                 (should (= level 5))
                 (should (eq color 'black))
                 (should (= limit 180))
                 (should (= increment 2))
                 (should-not fen)
                 (should-not text-mode)
                 (setq api-called t)
                 (funcall callback res-mock)))
              ((symbol-function 'lichess-game-play)
               (lambda (id)
                 (should (string= id "game123"))
                 (setq play-called t))))
      (lichess-ai--start-game 5 "black" 180 2)
      (should api-called)
      (should play-called)
      (should (member "Game started! ID: game123" messages))
      (should (member "Challenging Lichess AI level 5..." messages)))))

(ert-deftest lichess-ai-start-game-failure-test ()
  "Test that `lichess-ai--start-game` prints error message on failure."
  (let ((api-called nil)
        (messages '())
        (res-mock (lichess-http-result-err '(400 . ((error . "Invalid parameters"))))))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages)))
              ((symbol-function 'lichess-api-challenge-ai)
               (lambda (level color limit increment fen callback text-mode)
                 (setq api-called t)
                 (funcall callback res-mock)))
              ((symbol-function 'lichess-game-play)
               (lambda (_id)
                 (error "Should not be called"))))
      (lichess-ai--start-game 3 "white" 300 3)
      (should api-called)
      (should (member "Lichess AI error: 400 Invalid parameters" messages)))))

(provide 'lichess-ai-test)
;;; lichess-ai-test.el ends here
