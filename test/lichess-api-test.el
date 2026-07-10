;;; lichess-api-test.el --- Tests for lichess-api.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; Add project root to load-path
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory (or load-file-name (buffer-file-name))))))

(require 'lichess-http)
(require 'lichess-api)

;;; Code:

(ert-deftest lichess-api-dispatch-test ()
  "Test that Lichess API functions correctly route through `lichess-http-request`."
  (let* ((calls '())
         (mock-request (lambda (endpoint callback &rest plist)
                         (setq calls (cons (list endpoint callback plist) calls)))))
    (cl-letf (((symbol-function 'lichess-http-request) mock-request))
      ;; 1. GET TV channels
      (lichess-api-get-tv-channels (lambda (res)
                                     (should (lichess-http-result-success res))
                                     (should (string= (lichess-http-result-data res) "tv-channels-data"))))
      (should (= (length calls) 1))
      (should (string= (caar calls) "/api/tv/channels"))
      ;; Call callback with mock HTTP response
      (funcall (cadar calls) '(200 . "tv-channels-data"))

      ;; 2. POST AI challenge
      (setq calls '())
      (lichess-api-challenge-ai 5 'white 300 5 "some-fen" #'ignore nil)
      (should (= (length calls) 1))
      (should (string= (caar calls) "/api/challenge/ai"))
      (let ((plist (caddar calls)))
        (should (string= (plist-get plist :method) "POST"))
        (should (string-match-p "level=5" (plist-get plist :data)))
        (should (string-match-p "color=white" (plist-get plist :data)))
        (should (string-match-p "clock.limit=300" (plist-get plist :data)))
        (should (string-match-p "clock.increment=5" (plist-get plist :data)))
        (should (string-match-p "fen=some-fen" (plist-get plist :data)))
        (should (eq (plist-get plist :parse) 'json)))

      ;; 3. GET with error status
      (setq calls '())
      (lichess-api-get-tv-channels (lambda (res)
                                     (should-not (lichess-http-result-success res))
                                     (should (equal (lichess-http-result-error res) '(400 . "error-msg")))))
      (should (= (length calls) 1))
      (funcall (cadar calls) '(400 . "error-msg")))))

(ert-deftest lichess-api-seek-correspondence-test ()
  "Test that `lichess-api-board-seek-correspondence` correctly dispatches a POST request."
  (let* ((calls '())
         (mock-request (lambda (endpoint callback &rest plist)
                         (setq calls (cons (list endpoint callback plist) calls)))))
    (cl-letf (((symbol-function 'lichess-http-request) mock-request))
      (lichess-api-board-seek-correspondence 3 t 'standard 'white "1500-1800" #'ignore)
      (should (= (length calls) 1))
      (should (string= (caar calls) "/api/board/seek"))
      (let ((plist (caddar calls)))
        (should (string= (plist-get plist :method) "POST"))
        (should (string-match-p "days=3" (plist-get plist :data)))
        (should (string-match-p "rated=true" (plist-get plist :data)))
        (should (string-match-p "variant=standard" (plist-get plist :data)))
        (should (string-match-p "color=white" (plist-get plist :data)))
        (should (string-match-p "ratingRange=1500-1800" (plist-get plist :data)))))))

(ert-deftest lichess-api-board-seek-stream-test ()
  "Test that `lichess-api-board-seek-stream` correctly dispatches the POST stream request."
  (let* ((calls '()))
    (cl-letf (((symbol-function 'lichess-http-stream-open)
               (lambda (endpoint &rest plist)
                 (setq calls (cons (cons endpoint plist) calls))
                 "mock-stream-process")))
      (let ((stream
             (lichess-api-board-seek-stream
              15 10 "true" 'standard 'white "1500-1800"
              :on-event #'ignore)))
        (should (string= stream "mock-stream-process"))
        (should (= (length calls) 1))
        (should (string= (caar calls) "/api/board/seek"))
        (let ((plist (cdar calls)))
          (should (string= (plist-get plist :method) "POST"))
          (should (string-match-p "time=15" (plist-get plist :data)))
          (should (string-match-p "increment=10" (plist-get plist :data)))
          (should (string-match-p "rated=true" (plist-get plist :data)))
          (should (string-match-p "variant=standard" (plist-get plist :data)))
          (should (string-match-p "color=white" (plist-get plist :data)))
          (should (string-match-p "ratingRange=1500-1800" (plist-get plist :data)))
          (should (equal (plist-get plist :headers)
                         '(("Content-Type" . "application/x-www-form-urlencoded")))))))))

(provide 'lichess-api-test)
;;; lichess-api-test.el ends here
