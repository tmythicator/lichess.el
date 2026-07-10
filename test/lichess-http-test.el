;;; lichess-http-test.el --- Tests for lichess-http.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; Add project root to load-path
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory (or load-file-name (buffer-file-name))))))

(require 'lichess-http)

;;; Code:

(ert-deftest lichess-http-result-test ()
  "Test the `lichess-http-result` structures and constructors."
  (let ((ok-res (lichess-http-result-ok 'some-data))
        (err-res (lichess-http-result-err '(404 . "Not Found"))))
    (should (lichess-http-result-p ok-res))
    (should (lichess-http-result-success ok-res))
    (should (eq (lichess-http-result-data ok-res) 'some-data))
    (should-not (lichess-http-result-error ok-res))

    (should (lichess-http-result-p err-res))
    (should-not (lichess-http-result-success err-res))
    (should (equal (lichess-http-result-error err-res) '(404 . "Not Found")))
    (should-not (lichess-http-result-data err-res))))

(ert-deftest lichess-http-with-ok-test ()
  "Test the `lichess-http-with-ok` macro."
  (let ((ok-res (lichess-http-result-ok "hello"))
        (err-res (lichess-http-result-err '(500 . "Internal Error"))))
    ;; Successful case
    (let ((result
           (lichess-http-with-ok (val ok-res)
             (concat val " world"))))
      (should (string= result "hello world")))

    ;; Failure case (should propagate the error result without executing body)
    (let* ((body-executed nil)
           (result
            (lichess-http-with-ok (_val err-res)
              (setq body-executed t)
              "success")))
      (should-not body-executed)
      (should (lichess-http-result-p result))
      (should-not (lichess-http-result-success result))
      (should (equal (lichess-http-result-error result) '(500 . "Internal Error"))))))

(ert-deftest lichess-http-defendpoint-test ()
  "Test the `lichess-http-defendpoint` macro definition and behavior."
  (let* ((calls '())
         (mock-request (lambda (endpoint callback &rest plist)
                         (setq calls (cons (list endpoint callback plist) calls)))))
    (cl-letf (((symbol-function 'lichess-http-request) mock-request))
      ;; Define a temporary test endpoint
      (lichess-http-defendpoint lichess-http--test-endpoint "/api/test/:id"
        "Temporary endpoint for testing."
        :path-params (id)
        :query-params (param1 param2))

      ;; Call the defined endpoint
      (lichess-http--test-endpoint 123 "val1" nil (lambda (res)
                                                    (should (lichess-http-result-success res))))

      ;; Verify dispatch
      (should (= (length calls) 1))
      (should (string= (caar calls) "/api/test/123?param1=val1"))
      (let ((plist (caddar calls)))
        (should (string= (plist-get plist :method) "GET")))

      ;; Invoke the callback to complete
      (funcall (cadar calls) '(200 . "success-data")))))

(ert-deftest lichess-http-parse-ndjson-test ()
  "Test `lichess-http-parse-ndjson`."
  (let* ((data "{\"id\":\"user1\",\"name\":\"Alice\"}\n{\"id\":\"user2\",\"name\":\"Bob\"}\n\n{\"error\":\"some error\"}")
         (parsed (lichess-http-parse-ndjson data)))
    (should (= (length parsed) 3))
    (should (equal (cdr (assoc 'name (nth 0 parsed))) "Alice"))
    (should (equal (cdr (assoc 'name (nth 1 parsed))) "Bob"))
    (should (equal (cdr (assoc 'error (nth 2 parsed))) "some error")))
  (should (equal (lichess-http-parse-ndjson nil) nil))
  (should (equal (lichess-http-parse-ndjson "   \n  \n ") nil)))

(ert-deftest lichess-http-defendpoint-raw-test ()
  "Test `lichess-http-defendpoint` macro with :parse-type raw and :accept-header."
  (let* ((calls '())
         (mock-request (lambda (endpoint callback &rest plist)
                         (setq calls (cons (list endpoint callback plist) calls)))))
    (cl-letf (((symbol-function 'lichess-http-request) mock-request))
      ;; Define a temporary test endpoint
      (lichess-http-defendpoint lichess-http--test-raw-endpoint "/api/test-raw"
        "Temporary endpoint for testing raw NDJSON GET."
        :parse-type raw
        :accept-header "application/x-ndjson")

      ;; Call the defined endpoint
      (lichess-http--test-raw-endpoint (lambda (res)
                                         (should (lichess-http-result-success res))
                                         (should (equal (lichess-http-result-data res) "raw data"))))

      ;; Verify dispatch
      (should (= (length calls) 1))
      (should (string= (caar calls) "/api/test-raw"))
      (let ((plist (caddar calls)))
        (should (string= (plist-get plist :method) "GET"))
        (should (eq (plist-get plist :parse) 'raw))
        (should (string= (plist-get plist :accept) "application/x-ndjson")))

      ;; Invoke the callback to complete
      (funcall (cadar calls) '(200 . "raw data")))))

(ert-deftest lichess-http-stream-test ()
  "Test that `lichess-http-ndjson-open` and `lichess-http-ndjson-close` work with the struct."
  (let* ((mock-proc "mock-process-object")
         (network-stream-called nil)
         (sent-string nil)
         (deleted-proc nil))
    (cl-letf (((symbol-function 'open-network-stream)
               (lambda (&rest _args)
                 (setq network-stream-called t)
                 mock-proc))
              ((symbol-function 'set-process-query-on-exit-flag) #'ignore)
              ((symbol-function 'set-process-filter) #'ignore)
              ((symbol-function 'set-process-sentinel) #'ignore)
              ((symbol-function 'process-send-string)
               (lambda (_proc str)
                 (setq sent-string str)))
              ((symbol-function 'process-live-p) (lambda (proc) (eq proc mock-proc)))
              ((symbol-function 'delete-process) (lambda (proc) (setq deleted-proc proc))))

      (let ((stream (lichess-http-ndjson-open "/api/stream/test")))
        ;; 1. Check it's a valid struct
        (should (lichess-http-stream-p stream))
        (should (eq (lichess-http-stream-proc stream) mock-proc))
        (should (equal (lichess-http-stream-chunk-tail stream) ""))
        (should-not (lichess-http-stream-seen-headers stream))
        (should network-stream-called)
        (should (string-match-p "GET /api/stream/test HTTP/1.1" sent-string))

        ;; 2. Test closing it
        (lichess-http-ndjson-close stream)
        (should (eq deleted-proc mock-proc))))))

(ert-deftest lichess-http-stream-post-test ()
  "Test that `lichess-http-ndjson-open` supports POST, data, and headers."
  (let* ((mock-proc "mock-process-object")
         (network-stream-called nil)
         (sent-string nil)
         (deleted-proc nil))
    (cl-letf (((symbol-function 'open-network-stream)
               (lambda (&rest _args)
                 (setq network-stream-called t)
                 mock-proc))
              ((symbol-function 'set-process-query-on-exit-flag) #'ignore)
              ((symbol-function 'set-process-filter) #'ignore)
              ((symbol-function 'set-process-sentinel) #'ignore)
              ((symbol-function 'process-send-string)
               (lambda (_proc str)
                 (setq sent-string str)))
              ((symbol-function 'process-live-p) (lambda (proc) (eq proc mock-proc)))
              ((symbol-function 'delete-process) (lambda (proc) (setq deleted-proc proc))))

      (let ((stream (lichess-http-ndjson-open
                     "/api/board/seek"
                     :method "POST"
                     :data "time=10&increment=0"
                     :headers '(("Content-Type" . "application/x-www-form-urlencoded")))))
        ;; Check it's a valid struct
        (should (lichess-http-stream-p stream))
        (should (eq (lichess-http-stream-proc stream) mock-proc))
        (should network-stream-called)
        
        ;; Check sent HTTP request headers and body
        (should (string-match-p "POST /api/board/seek HTTP/1.1" sent-string))
        (should (string-match-p "Content-Type: application/x-www-form-urlencoded" sent-string))
        (should (string-match-p "Content-Length: 19" sent-string))
        (should (string-match-p "time=10&increment=0" sent-string))

        (lichess-http-ndjson-close stream)
        (should (eq deleted-proc mock-proc))))))

(ert-deftest lichess-http-defstream-test ()
  "Test the `lichess-http-defstream` macro definition and behavior."
  (let* ((calls '()))
    (cl-letf (((symbol-function 'lichess-http-stream-open)
               (lambda (endpoint &rest plist)
                 (setq calls (cons (cons endpoint plist) calls))
                 "mock-stream-object")))
      ;; Define a temporary test stream endpoint
      (lichess-http-defstream lichess-http--test-stream "/api/test-stream/:id"
        "Temporary stream for testing."
        :method POST
        :path-params (id)
        :post-params (param1 param2))

      ;; Call the defined stream function
      (let ((stream
             (lichess-http--test-stream
              123 "val1" nil
              :on-event (lambda (_) nil))))
        (should (string= stream "mock-stream-object"))
        (should (= (length calls) 1))
        (should (string= (caar calls) "/api/test-stream/123"))
        (let ((plist (cdar calls)))
          (should (string= (plist-get plist :method) "POST"))
          (should (string= (plist-get plist :data) "param1=val1"))
          (should (equal (plist-get plist :headers)
                         '(("Content-Type" . "application/x-www-form-urlencoded"))))
          (should (functionp (plist-get plist :on-event))))))))

(provide 'lichess-http-test)
;;; lichess-http-test.el ends here
