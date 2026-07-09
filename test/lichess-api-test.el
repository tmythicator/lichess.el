;;; lichess-api-test.el --- Tests for lichess-api.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; Add project root to load-path
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory (or load-file-name (buffer-file-name))))))

(require 'lichess-api)

(ert-deftest lichess-api-dispatch-test ()
  "Test that API functions route requests through `lichess-api-request-function`."
  (let* ((calls '())
         (lichess-api-request-function
          (lambda (endpoint callback &rest plist)
            (setq calls (cons (list endpoint callback plist) calls)))))

    ;; 1. GET TV channels
    (lichess-api-get-tv-channels #'ignore)
    (should (= (length calls) 1))
    (should (string= (caar calls) "/api/tv/channels"))
    (should (eq (cadar calls) #'ignore))
    (should (equal (caddar calls) '(:method "GET" :accept "application/json" :headers nil :parse json :anonymous nil)))

    ;; 2. POST AI challenge
    (setq calls '())
    (lichess-api-challenge-ai 5 'white 300 5 "some-fen" #'ignore nil)
    (should (= (length calls) 1))
    (should (string= (caar calls) "/api/challenge/ai"))
    (should (eq (cadar calls) #'ignore))
    (let ((plist (caddar calls)))
      (should (string= (plist-get plist :method) "POST"))
      (should (string-match-p "level=5" (plist-get plist :data)))
      (should (string-match-p "color=white" (plist-get plist :data)))
      (should (string-match-p "clock.limit=300" (plist-get plist :data)))
      (should (string-match-p "clock.increment=5" (plist-get plist :data)))
      (should (string-match-p "fen=some-fen" (plist-get plist :data)))
      (should (eq (plist-get plist :parse) 'json)))))

(ert-deftest lichess-token-resolution-test ()
  "Test that `lichess-token` resolves correctly using the variable and auth-source."
  (let ((lichess-token nil))
    ;; 1. If lichess-token is nil and auth-source returns nil, it should be nil
    (cl-letf (((symbol-function 'auth-source-search) (lambda (&rest _args) nil)))
      (should-not (lichess-token)))

    ;; 2. If lichess-token variable is set, it should return that variable
    (let ((lichess-token "custom-token-val"))
      (cl-letf (((symbol-function 'auth-source-search) (lambda (&rest _args) (error "Should not be called"))))
        (should (string= (lichess-token) "custom-token-val"))))

    ;; 3. If lichess-token variable is nil, it should look up via auth-source
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest args)
                 (should (equal (plist-get args :host) "lichess.org"))
                 (should (equal (plist-get args :require) '(:secret)))
                 '((:host "lichess.org" :secret "auth-source-token-val")))))
      (should (string= (lichess-token) "auth-source-token-val")))

    ;; 4. If auth-source returns a function for secret, it should execute it
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _args)
                 '((:host "lichess.org" :secret (lambda () "auth-source-func-token-val"))))))
      (should (string= (lichess-token) "auth-source-func-token-val")))))

(provide 'lichess-api-test)
;;; lichess-api-test.el ends here
