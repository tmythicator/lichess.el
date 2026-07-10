;;; lichess-broadcast-view-test.el --- Tests for lichess-broadcast-view.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; Add project root to load-path
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory (or load-file-name (buffer-file-name))))))

(require 'lichess-broadcast-view)

;;; Code:

(ert-deftest lichess-broadcast-view-watch-round-init-test ()
  "Test `lichess-broadcast-view-watch-round` initializes state and timer."
  (let ((api-called nil)
        (timer-scheduled nil)
        (round-id "round123")
        (url "https://lichess.org/api/broadcast/round/round123.pgn"))
    (cl-letf (((symbol-function 'lichess-api-get-broadcast-round)
               (lambda (req-url callback)
                 (should (string= req-url url))
                 (setq api-called t)
                 (should (functionp callback))))
              ((symbol-function 'run-at-time)
               (lambda (time repeat func)
                 (should (= time 5))
                 (should-not repeat)
                 (should (functionp func))
                 (setq timer-scheduled t)
                 'mock-timer))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'pop-to-buffer) #'identity))
      (lichess-broadcast-view-watch-round round-id url)
      (let ((buf (get-buffer "*Lichess Broadcast: round123*")))
        (should buf)
        (should api-called)
        (should timer-scheduled)
        (with-current-buffer buf
          (should (eq major-mode 'lichess-broadcast-view-mode))
          (should (equal (plist-get lichess-broadcast-view--state :round-id) round-id))
          (should (equal (plist-get lichess-broadcast-view--state :url) url))
          (should (eq (plist-get lichess-broadcast-view--state :timer) 'mock-timer)))
        (kill-buffer buf)))))

(ert-deftest lichess-broadcast-view-render-game-block-tui-test ()
  "Test that `lichess-broadcast-view--render-game-block` returns formatted TUI strings."
  (let* ((game '((players . ((white . ((user . ((name . "PlayerA"))) (rating . 2400)))
                             (black . ((user . ((name . "PlayerB"))) (rating . 2300)))))
                 (fen . "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1")))
         (block (lichess-broadcast-view--render-game-block game 40 :style :tui)))
    (should (listp block))
    (should (string= (car block) "PlayerA (2400) vs PlayerB (2300)"))
    ;; The rest should be board TUI lines
    (should (member "|♜|♞|♝|♛|♚|♝|♞|♜|8" block))
    (should (member "|·|·|·|·|·|·|·|·|3" block))))

(ert-deftest lichess-broadcast-view-render-game-block-invalid-fen-test ()
  "Test that `lichess-broadcast-view--render-game-block` handles invalid FEN gracefully."
  (let* ((game '((players . ((white . ((user . ((name . "PlayerA"))) (rating . 2400)))
                             (black . ((user . ((name . "PlayerB"))) (rating . 2300)))))
                 (fen . "invalid-fen-string")))
         (block (lichess-broadcast-view--render-game-block game 40 :style :tui)))
    (should (listp block))
    (should (string= (car block) "PlayerA (2400) vs PlayerB (2300)"))
    (should (member "[Invalid Position]" block))))

(ert-deftest lichess-broadcast-view-cleanup-timer-test ()
  "Test that `lichess-broadcast-view--cleanup-timer` cancels the active timer."
  (let ((timer-cancelled nil)
        (buf (get-buffer-create "*Lichess Broadcast: cleanuptest*")))
    (cl-letf (((symbol-function 'cancel-timer)
               (lambda (timer)
                 (should (eq timer 'mock-timer))
                 (setq timer-cancelled t))))
      (with-current-buffer buf
        (lichess-broadcast-view-mode)
        (setq lichess-broadcast-view--state (list :timer 'mock-timer))
        (kill-buffer buf))
      (should timer-cancelled))))

(ert-deftest lichess-broadcast-view-handle-update-success-test ()
  "Test that `lichess-broadcast-view--handle-update` renders grid on success."
  (let ((buf (get-buffer-create "*Lichess Broadcast: round555*"))
        (game1 '((players . ((white . ((name . "A"))) (black . ((name . "B")))))))
        (game2 '((players . ((white . ((name . "C"))) (black . ((name . "D"))))))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (lichess-broadcast-view-mode)
        (erase-buffer)
        (setq lichess-broadcast-view--state (list :round-id "round555" :url "http://mock"))))
    (let ((res (lichess-http-result-ok `((games . (,game1 ,game2))))))
      (lichess-broadcast-view--handle-update res "round555")
      (with-current-buffer buf
        (goto-char (point-min))
        (should (search-forward "Round Status: 2 games active" nil t))
        (should (search-forward "A vs B" nil t))
        (should (search-forward "C vs D" nil t))
        (should (equal (plist-get lichess-broadcast-view--state :games) (list game1 game2)))))
    (kill-buffer buf)))

(ert-deftest lichess-broadcast-view-handle-update-failure-test ()
  "Test that `lichess-broadcast-view--handle-update` displays error message on failure."
  (let ((buf (get-buffer-create "*Lichess Broadcast: round666*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (lichess-broadcast-view-mode)
        (erase-buffer)
        (setq lichess-broadcast-view--state (list :round-id "round666"))))
    (let ((res (lichess-http-result-err '(500 . "Internal Server Error"))))
      (lichess-broadcast-view--handle-update res "round666")
      (with-current-buffer buf
        (goto-char (point-min))
        (should (search-forward "Error fetching broadcast round666: 500" nil t))))
    (kill-buffer buf)))

(provide 'lichess-broadcast-view-test)
;;; lichess-broadcast-view-test.el ends here
