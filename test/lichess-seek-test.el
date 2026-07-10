;;; lichess-seek-test.el --- Tests for lichess-seek.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; Add project root to load-path
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory (or load-file-name (buffer-file-name))))))

(require 'lichess-seek)

;;; Code:

(ert-deftest lichess-seek-real-time-start-test ()
  "Test that `lichess-seek--real-time` opens the correct stream and sets up active handles."
  (let ((event-stream-started nil)
        (seek-stream-args nil)
        (lichess-seek--active-stream nil))
    (cl-letf (((symbol-function 'lichess-challenge--listen-for-start)
               (lambda () (setq event-stream-started t)))
              ((symbol-function 'lichess-api-board-seek-stream)
               (lambda (&rest args)
                 (setq seek-stream-args args)
                 'mock-active-stream-process)))
      (lichess-seek--real-time 15 10 t "white" "standard" "1500-1800")
      (should event-stream-started)
      (should (eq lichess-seek--active-stream 'mock-active-stream-process))
      (should (equal (cl-subseq seek-stream-args 0 6)
                     '(15 10 "true" "standard" "white" "1500-1800"))))))

(ert-deftest lichess-seek-cancel-test ()
  "Test that `lichess-seek-cancel` correctly closes and clears the stream."
  (let ((stream-closed nil)
        (lichess-seek--active-stream 'mock-active-stream-process)
        (lichess-seek--cancelling nil))
    (cl-letf (((symbol-function 'lichess-http-stream-close)
               (lambda (stream)
                 (should (eq stream 'mock-active-stream-process))
                 (should lichess-seek--cancelling)
                 (setq stream-closed t))))
      (lichess-seek-cancel)
      (should stream-closed)
      (should-not lichess-seek--active-stream))))

(provide 'lichess-seek-test)
;;; lichess-seek-test.el ends here
