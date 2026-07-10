;;; lichess-challenge-test.el --- Tests for lichess-challenge.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; Add project root to load-path
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory (or load-file-name (buffer-file-name))))))

(require 'lichess-challenge)

;;; Code:

(ert-deftest lichess-challenge-get-friends-success-test ()
  "Test `lichess-challenge--get-friends` on successful response."
  (let ((friends-res nil))
    (cl-letf (((symbol-function 'lichess-api-get-following)
               (lambda (callback)
                 (funcall callback
                          (lichess-http-result-ok
                           "{\"id\":\"alice\",\"name\":\"Alice\"}\n{\"id\":\"bob\",\"name\":\"Bob\"}\n")))))
      (lichess-challenge--get-friends
       (lambda (type friends)
         (should (eq type :ok))
         (setq friends-res friends)))
      (should (equal friends-res '(("Alice" . "alice") ("Bob" . "bob")))))))

(ert-deftest lichess-challenge-get-friends-missing-scope-test ()
  "Test `lichess-challenge--get-friends` handles 403 missing scope."
  (let ((result-type nil))
    (cl-letf (((symbol-function 'lichess-api-get-following)
               (lambda (callback)
                 (funcall callback (lichess-http-result-err '(403 . "Forbidden"))))))
      (lichess-challenge--get-friends
       (lambda (type _friends)
         (setq result-type type)))
      (should (eq result-type :missing-scope)))))

(ert-deftest lichess-challenge-get-friends-error-test ()
  "Test `lichess-challenge--get-friends` handles other API errors."
  (let ((result-type nil)
        (messages '()))
    (cl-letf (((symbol-function 'lichess-api-get-following)
               (lambda (callback)
                 (funcall callback (lichess-http-result-err '(500 . "Error")))))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (lichess-challenge--get-friends
       (lambda (type _friends)
         (setq result-type type)))
      (should (eq result-type :error))
      (should (member "Error fetching friends: 500" messages)))))

(ert-deftest lichess-challenge-send-success-test ()
  "Test `lichess-challenge--send` on successful api call."
  (let ((api-called nil)
        (listener-started nil)
        (list-called nil)
        (messages '()))
    (cl-letf (((symbol-function 'lichess-api-challenge-user)
               (lambda (username rated color limit increment variant callback)
                 (should (string= username "opponent1"))
                 (should rated)
                 (should (eq color 'white))
                 (should (= limit 300))
                 (should (= increment 3))
                 (should (string= variant "standard"))
                 (setq api-called t)
                 (funcall callback (lichess-http-result-ok 'success))))
              ((symbol-function 'lichess-challenge--listen-for-start)
               (lambda () (setq listener-started t)))
              ((symbol-function 'lichess-challenge-list)
               (lambda () (setq list-called t)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (lichess-challenge--send "opponent1" t "white" 300 3 "standard")
      (should api-called)
      (should listener-started)
      (should list-called)
      (should (member "Challenge (standard) sent to opponent1! Waiting for acceptance..." messages)))))

(ert-deftest lichess-challenge-send-failure-test ()
  "Test `lichess-challenge--send` on API error."
  (let ((api-called nil)
        (messages '()))
    (cl-letf (((symbol-function 'lichess-api-challenge-user)
               (lambda (_username _rated _color _limit _increment _variant callback)
                 (setq api-called t)
                 (funcall callback (lichess-http-result-err '(400 . ((error . "Lichess error description"))))))))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) messages)))
                ((symbol-function 'lichess-challenge--listen-for-start)
                 (lambda () (error "Should not be called")))
                ((symbol-function 'lichess-challenge-list)
                 (lambda () (error "Should not be called"))))
        (lichess-challenge--send "opponent1" t "white" 300 3 "standard")
        (should api-called)
        (should (member "Error challenging opponent1: 400 Lichess error description" messages))))))

(ert-deftest lichess-challenge-handle-event-game-start-test ()
  "Test that event type gameStart cleans up listener and starts game play."
  (let ((stream-closed nil)
        (game-play-id nil)
        (lichess-challenge--event-stream 'mock-event-stream)
        (messages '()))
    (cl-letf (((symbol-function 'lichess-http-stream-close)
               (lambda (stream)
                 (should (eq stream 'mock-event-stream))
                 (setq stream-closed t)))
              ((symbol-function 'lichess-game-play)
               (lambda (id)
                 (setq game-play-id id)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (lichess-challenge--handle-event
       '((type . "gameStart")
         (game . ((id . "newgame123")))))
      (should stream-closed)
      (should (string= game-play-id "newgame123"))
      (should-not lichess-challenge--event-stream)
      (should (member "Game started! ID: newgame123" messages)))))

(ert-deftest lichess-challenge-handle-event-canceled-test ()
  "Test that event type challengeCanceled cleans up listener."
  (let ((stream-closed nil)
        (lichess-challenge--event-stream 'mock-event-stream)
        (messages '()))
    (cl-letf (((symbol-function 'lichess-http-stream-close)
               (lambda (stream)
                 (should (eq stream 'mock-event-stream))
                 (setq stream-closed t)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (lichess-challenge--handle-event
       '((type . "challengeCanceled")
         (challenge . ((id . "ch123")))))
      (should stream-closed)
      (should-not lichess-challenge--event-stream)
      (should (member "Challenge ch123 canceled." messages)))))

(ert-deftest lichess-challenge-handle-event-declined-test ()
  "Test that event type challengeDeclined cleans up listener."
  (let ((stream-closed nil)
        (lichess-challenge--event-stream 'mock-event-stream)
        (messages '()))
    (cl-letf (((symbol-function 'lichess-http-stream-close)
               (lambda (stream)
                 (should (eq stream 'mock-event-stream))
                 (setq stream-closed t)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (lichess-challenge--handle-event
       '((type . "challengeDeclined")
         (challenge . ((id . "ch123")
                       (destUser . ((name . "OpponentPlayer")))))))
      (should stream-closed)
      (should-not lichess-challenge--event-stream)
      (should (member "Challenge ch123 declined by OpponentPlayer." messages)))))

(provide 'lichess-challenge-test)
;;; lichess-challenge-test.el ends here
