;;; lichess-tv-test.el --- Tests for lichess-tv.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; Add project root to load-path
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory (or load-file-name (buffer-file-name))))))

(require 'lichess-tv)

;;; Code:

(ert-deftest lichess-tv-init-test ()
  "Test that `lichess-tv` creates buffer and calls channels API."
  (let ((channels-api-called nil)
        (buf (get-buffer lichess-tv--buf)))
    (when buf (kill-buffer buf))
    (cl-letf (((symbol-function 'lichess-api-get-tv-channels)
               (lambda (callback)
                 (should (functionp callback))
                 (setq channels-api-called t))))
      (lichess-tv)
      (should channels-api-called)
      (let ((new-buf (get-buffer lichess-tv--buf)))
        (should new-buf)
        (with-current-buffer new-buf
          (should (eq major-mode 'lichess-tv-mode))
          (goto-char (point-min))
          (should (search-forward "Fetching Lichess TV channels…" nil t)))
        (kill-buffer new-buf)))))

(ert-deftest lichess-tv-handle-channels-success-test ()
  "Test `lichess-tv--handle-channels` with successful data."
  (let ((buf (get-buffer-create lichess-tv--buf))
        (fetched-games '())
        (lichess-tv--next-at 0.0))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (lichess-tv-mode)
        (erase-buffer)))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_time _repeat seq-func)
                 ;; Run the fetch function synchronously for tests
                 (funcall seq-func)
                 nil))
              ((symbol-function 'lichess-api-get-game)
               (lambda (id callback)
                 (push id fetched-games)
                 ;; mock success response
                 (funcall callback
                          (lichess-http-result-ok
                           `((id . ,id)
                             (players . ((white . ((user . ((name . "Alice"))) (rating . 2200)))
                                         (black . ((user . ((name . "Bob"))) (rating . 2100)))))))))))
      (let ((res (lichess-http-result-ok
                  '((blitz . ((gameId . "blitz123")))
                    (bullet . ((id . "bullet456")))))))
        (lichess-tv--handle-channels res)
        (should (equal (reverse fetched-games) '("blitz123" "bullet456")))
        (with-current-buffer buf
          (goto-char (point-min))
          (should (search-forward "blitz" nil t))
          (should (search-forward "Alice (2200)  vs  Bob (2100)" nil t))
          (should (search-forward "id:blitz123" nil t))
          (goto-char (point-min))
          (should (search-forward "bullet" nil t))
          (should (search-forward "Alice (2200)  vs  Bob (2100)" nil t))
          (should (search-forward "id:bullet456" nil t)))))
    (kill-buffer buf)))

(ert-deftest lichess-tv-handle-channels-failure-test ()
  "Test `lichess-tv--handle-channels` on API failure."
  (let ((buf (get-buffer-create lichess-tv--buf)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (let ((res (lichess-http-result-err '(404 . "Not Found"))))
      (lichess-tv--handle-channels res)
      (with-current-buffer buf
        (goto-char (point-min))
        (should (search-forward "HTTP 404 from /api/tv/channels" nil t))))
    (kill-buffer buf)))

(ert-deftest lichess-tv-update-line-test ()
  "Test `lichess-tv--update-line` correctly locates and updates a line by ID or marker."
  (let ((buf (get-buffer-create lichess-tv--buf)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (lichess-tv-mode)
        (erase-buffer)
        (let* ((m1 (lichess-util--insert-propertized-line "blitz         loading…  id:1" "1"))
               (m2 (lichess-util--insert-propertized-line "bullet        loading…  id:2" "2")))
          ;; 1. Update m1 by marker
          (lichess-tv--update-line m1 "blitz         PlayerA vs PlayerB  id:1" "1")
          (goto-char (point-min))
          (should (search-forward "blitz" nil t))
          (should (search-forward "PlayerA vs PlayerB" nil t))
          (should (search-forward "id:1" nil t))
          (should (equal (get-text-property (point-at-bol) 'lichess-game-id) "1"))

          ;; 2. Update m2 by ID fallback (simulating stale marker)
          (set-marker m2 nil) ;; invalidate marker
          (lichess-tv--update-line nil "bullet        PlayerC vs PlayerD  id:2" "2")
          (goto-char (point-min))
          (should (search-forward "bullet" nil t))
          (should (search-forward "PlayerC vs PlayerD" nil t))
          (should (search-forward "id:2" nil t))
          (should (equal (get-text-property (point-at-bol) 'lichess-game-id) "2")))))
    (kill-buffer buf)))

(ert-deftest lichess-tv-watch-game-at-point-test ()
  "Test that `lichess-tv-watch-game-at-point` triggers watch if game ID is present."
  (let ((watch-id nil)
        (messages '()))
    (cl-letf (((symbol-function 'lichess-game-watch)
               (lambda (id) (setq watch-id id)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (with-temp-buffer
        ;; 1. Call on line with no game ID
        (insert "some line without game id\n")
        (goto-char (point-min))
        (lichess-tv-watch-game-at-point)
        (should-not watch-id)
        (should (member "No game ID found on this line." messages))

        ;; 2. Call on line with game ID propertized
        (erase-buffer)
        (lichess-util--insert-propertized-line "blitz         Alice vs Bob  id:game1" "game1")
        (goto-char (point-min))
        (lichess-tv-watch-game-at-point)
        (should (string= watch-id "game1"))))))

(provide 'lichess-tv-test)
;;; lichess-tv-test.el ends here
