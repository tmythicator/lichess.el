;;; lichess-util-fmt-test.el --- Tests for player name formatting -*- lexical-binding: t; -*-

(require 'ert)
(require 'lichess-util)

(ert-deftest lichess-util-fmt-test-simple ()
  "Test simple name/rating structure."
  (let ((obj '((name . "Player") (rating . 1500))))
    (should (string= (lichess-util-fmt-user-obj obj) "Player (1500)"))))

(ert-deftest lichess-util-fmt-test-nested ()
  "Test nested user object with title."
  (let ((obj '((user . ((name . "GrandMaster") (title . "GM"))) (rating . 2800))))
    (should (string= (lichess-util-fmt-user-obj obj) "GM GrandMaster (2800)"))))

(ert-deftest lichess-util-fmt-test-username-fallback ()
  "Test fallback to username inside user object."
  (let ((obj '((user . ((username . "User123"))))))
    (should (string= (lichess-util-fmt-user-obj obj) "User123"))))

(ert-deftest lichess-util-fmt-test-anonymous ()
  "Test anonymous fallback."
  (should (string= (lichess-util-fmt-user-obj nil) "Anonymous"))
  (should (string= (lichess-util-fmt-user-obj '((foo . "bar"))) "Anonymous")))

(ert-deftest lichess-util-fmt-test-ai ()
  "Test AI level formatting."
  (let ((obj1 '((aiLevel . 3)))
        (obj2 '((ai . 8))))
    (should (string= (lichess-util-fmt-user-obj obj1) "Stockfish level 3"))
    (should (string= (lichess-util-fmt-user-obj obj2) "Stockfish level 8"))))

(ert-deftest lichess-util-fmt-test-flat-name ()
  "Test flat player object with name but no userId (e.g. from game t7HAF0vX)."
  (let ((obj '((userId . nil) (rating . 1832) (name . "garmonbozya"))))
    (should (string= (lichess-util-fmt-user-obj obj) "garmonbozya (1832)"))))
