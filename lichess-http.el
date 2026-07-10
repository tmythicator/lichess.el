;;; lichess-http.el --- HTTP/JSON/NDJSON helpers for Lichess -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026  Alexandr Timchenko
;; URL: https://github.com/tmythicator/Lichess.el
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;; Centralized helpers:
;; - `lichess-http-request'    : generic async HTTP, JSON parsing
;; - `lichess-http-json'       : simple GET JSON convenience
;; - `lichess-http-ndjson-open': open NDJSON stream (handles headers+chunked)
;; - `lichess-http-ndjson-close': close a previously opened stream
;;
;; All functions are callback-based and non-blocking.
;;
;;; Code:

(require 'cl-lib)
(require 'url)
(require 'json)
(require 'subr-x)

(declare-function lichess-token "lichess")

;;;; Header helpers
(defun lichess-http--auth-header-line ()
  "Return raw Authorization header line for manual sockets, or \"\"."
  (let ((token (lichess-token)))
    (if (and token (stringp token) (> (length token) 0))
        (format "Authorization: Bearer %s\r\n" token)
      "")))

(defun lichess-http--auth-headers (&optional extra accept)
  "Return an alist of headers.  Add Authorization when `lichess-token' is set.
EXTRA (alist) is appended.  ACCEPT, when non-nil, sets Accept header."
  (let ((token (lichess-token)))
    (append
     (when (and token (stringp token) (> (length token) 0))
       `(("Authorization" . ,(concat "Bearer " token))))
     (when accept
       `(("Accept" . ,accept)))
     extra)))

(defun lichess-http--abs-url (url-or-endpoint)
  "Return absolute URL for URL-OR-ENDPOINT (prepend https://lichess.org if needed)."
  (if (string-prefix-p "http" url-or-endpoint)
      url-or-endpoint
    (concat "https://lichess.org" url-or-endpoint)))

(defun lichess-http--ensure-utf8 (str)
  "Ensure STR is decoded as UTF-8.
If STR contains only byte-range characters (0-255) but has high-bit chars,
it is likely raw bytes interpreted as Latin-1 by `url-retrieve'.
In that case, we decode it as UTF-8.
If STR already contains wide characters (> 255), we return it as-is."
  (if (and (not (string-match-p "[^\x00-\xff]" str))
           (string-match-p "[\x80-\xff]" str))
      (decode-coding-string str 'utf-8)
    str))

;;;; Core async request
(defun lichess-http-request (url-or-endpoint callback &rest plist)
  "Perform an async HTTP request and call CALLBACK with a cons.
The status/value is (STATUS . JSON/STRING/NIL).

URL-OR-ENDPOINT can be absolute or like \"/api/tv/channels\".

PLIST keys:
  :method   HTTP method string (default \"GET\")
  :headers  Alist of headers to add
            (Authorization is auto if `lichess-token' set)
  :accept   Accept header (e.g., \"application/json\")
  :data     Request body string (UTF-8)
  :parse    One of: \='json (default), \='raw (return raw body string)
  :anonymous If non-nil, do not send Authorization header

CALLBACK receives (STATUS . VALUE), where VALUE is:
  - parsed JSON object when :parse is \='json and parsing succeeds
  - nil if JSON parsing fails
  - raw string when :parse is \='raw."
  (let* ((method (or (plist-get plist :method) "GET"))
         (headers (plist-get plist :headers))
         (accept (plist-get plist :accept))
         (data (plist-get plist :data))
         (parse (or (plist-get plist :parse) 'json))
         (anon (plist-get plist :anonymous))
         (url-request-method method)
         (url-request-extra-headers
          (if anon
              (append
               (when accept
                 `(("Accept" . ,accept)))
               headers)
            (lichess-http--auth-headers headers accept)))
         (url-request-data
          (when data
            (encode-coding-string data 'utf-8)))
         (abs (lichess-http--abs-url url-or-endpoint)))
    (url-retrieve
     abs
     (lambda (_)
       (let ((temp-buf (current-buffer))
             (status
              (or (bound-and-true-p url-http-response-status) 0)))
         (goto-char
          (or (bound-and-true-p url-http-end-of-headers) (point-min)))
         (pcase parse
           ('raw
            (let ((body
                   (buffer-substring-no-properties
                    (point) (point-max))))
              (funcall callback (cons status body))))
           (_
            (let* ((raw-body
                    (buffer-substring-no-properties
                     (point) (point-max)))
                   (body (lichess-http--ensure-utf8 raw-body))
                   (json
                    (condition-case _
                        (let ((json-object-type 'alist)
                              (json-array-type 'list))
                          (json-read-from-string body))
                      (error
                       nil))))
              (funcall callback (cons status json)))))
         ;; Kill the temp buffer; we saved it above in case callback changed current buffer
         (when (buffer-live-p temp-buf)
           (kill-buffer temp-buf))))
     nil t)))

(defun lichess-http-json
    (url-or-endpoint callback &optional headers anonymous)
  "GET JSON from URL-OR-ENDPOINT and call CALLBACK with a `lichess-http-result'.
HEADERS is an alist to add
Authorization is added automatically unless ANONYMOUS is non-nil."
  (lichess-http-request url-or-endpoint
                        (lambda (res-cons)
                          (let* ((status (car res-cons))
                                 (val (cdr res-cons))
                                 (res
                                  (if (and (>= status 200)
                                           (< status 300))
                                      (lichess-http-result-ok val)
                                    (lichess-http-result-err
                                     (cons status val)))))
                            (funcall callback res)))
                        :method "GET"
                        :accept "application/json"
                        :headers headers
                        :parse 'json
                        :anonymous anonymous))

;;;; NDJSON streaming (manual TLS socket)

(cl-defstruct
 lichess-http-stream
 "Structure representing a Lichess HTTP NDJSON connection stream."
 proc
 buf
 seen-headers
 chunk-tail)

(defun lichess-http--chunk-size-line-p (line)
  "Non-nil if LINE look like an HTTP/1.1 chunk-size marker."
  (or (string-match-p "\\`[0-9A-Fa-f]+\\(?:;.*\\)?\\'" line)
      (string-match-p "\\`[0-9]+\\'" line)))

(cl-defun
 lichess-http-ndjson-open
 (url-or-endpoint
  &key buffer-name
  on-event ;; (lambda (obj))
  on-open ;; (lambda (proc buf))
  on-close ;; (lambda (proc msg))
  method data headers)
 "Open an NDJSON stream to URL-OR-ENDPOINT and return a \`lichess-http-stream'.

Arguments:
  BUFFER-NAME  Name for the process buffer (created if missing).
  ON-EVENT     Called with one parsed JSON object per line.
  ON-OPEN      Called once when the socket is connected.
  ON-CLOSE     Called when the process terminates; receives (PROC MSG).
  METHOD       HTTP method string (default \"GET\").
  DATA         Request body string (UTF-8) to send.
  HEADERS      Alist of extra headers to add."
 (let* ((buf (get-buffer-create (or buffer-name "*Lichess NDJSON*")))
        (stream
         (make-lichess-http-stream
          :buf buf
          :chunk-tail ""
          :seen-headers nil))
        (proc
         (open-network-stream
          (format "lichess-ndjson-%x" (random))
          buf
          "lichess.org"
          443
          :type 'tls
          :coding 'binary)))
   (setf (lichess-http-stream-proc stream) proc)
   (set-process-query-on-exit-flag proc nil)
   (with-current-buffer buf
     (special-mode))
   (set-process-filter
    proc
    (lambda (_proc chunk)
      ;; Accumulate and strip headers once
      (setf (lichess-http-stream-chunk-tail stream)
            (concat (lichess-http-stream-chunk-tail stream) chunk))
      (unless (lichess-http-stream-seen-headers stream)
        (let ((hdr-end
               (string-match
                "\r?\n\r?\n"
                (lichess-http-stream-chunk-tail stream))))
          (when hdr-end
            (setf (lichess-http-stream-seen-headers stream) t)
            (setf (lichess-http-stream-chunk-tail stream)
                  (substring (lichess-http-stream-chunk-tail stream)
                             (+ hdr-end
                                (length
                                 (match-string
                                  0
                                  (lichess-http-stream-chunk-tail
                                   stream)))))))))
      (when (lichess-http-stream-seen-headers stream)
        (let ((lines
               (split-string (lichess-http-stream-chunk-tail stream)
                             "\n")))
          (dotimes (i (max 0 (1- (length lines))))
            (let ((line (string-trim (nth i lines))))
              (cond
               ((or (string-empty-p line)
                    (string-prefix-p ":" line)) ;; SSE comments
                nil)
               ((lichess-http--chunk-size-line-p line)
                nil)
               (t
                (condition-case _
                    (let* ((json-object-type 'alist)
                           (json-array-type 'list)
                           (decoded
                            (decode-coding-string line 'utf-8))
                           (obj (json-read-from-string decoded)))
                      (when (functionp on-event)
                        (funcall on-event obj)))
                  (error
                   nil))))))
          (setf (lichess-http-stream-chunk-tail stream)
                (car (last lines)))))))
   (set-process-sentinel
    proc
    (lambda (p msg)
      (when (functionp on-close)
        (funcall on-close p (string-trim msg)))))
   ;; Send HTTP request
   (let* ((path
           (if (string-prefix-p "http" url-or-endpoint)
               ;; extract path from absolute URL
               (let ((u (url-generic-parse-url url-or-endpoint)))
                 (concat
                  (or (url-filename u) "/")
                  (let ((q (url-target u)))
                    (or q ""))))
             url-or-endpoint))
          (m (or method "GET"))
          (h-alist headers)
          (extra-headers-str
           (if h-alist
               (mapconcat (lambda (hdr)
                            (format "%s: %s\r\n" (car hdr) (cdr hdr)))
                          h-alist
                          "")
             ""))
          (encoded-data
           (and data (encode-coding-string data 'utf-8))))
     (process-send-string
      proc
      (concat
       (format "%s %s HTTP/1.1\r\n" m path)
       "Host: lichess.org\r\n"
       "User-Agent: Emacs\r\n"
       "Accept: application/x-ndjson\r\n"
       (lichess-http--auth-header-line)
       extra-headers-str
       "Connection: keep-alive\r\n"
       (if encoded-data
           (format "Content-Length: %d\r\n\r\n%s"
                   (length encoded-data)
                   encoded-data)
         "\r\n"))))
   (when (functionp on-open)
     (funcall on-open proc buf))
   stream))

(defun lichess-http-ndjson-close (stream)
  "Close STREAM returned by `lichess-http-ndjson-open'."
  (when (and stream (lichess-http-stream-p stream))
    (let ((proc (lichess-http-stream-proc stream)))
      (when (process-live-p proc)
        (delete-process proc)))))

;;;; Result Monad Struct and Helpers

(cl-defstruct
 lichess-http-result
 "A monadic container representing the result of an API call."
 (success nil :read-only t)
 data
 error)

(defun lichess-http-result-ok (data)
  "Create a successful Lichess API result containing DATA."
  (make-lichess-http-result :success t :data data))

(defun lichess-http-result-err (err)
  "Create a failed Lichess API result containing ERR."
  (make-lichess-http-result :success nil :error err))

(defun lichess-http-parse-ndjson (data)
  "Parse a raw NDJSON string DATA into a list of parsed JSON objects."
  (when (stringp data)
    (let ((lines (split-string (string-trim data) "\n" t))
          (json-object-type 'alist)
          (json-array-type 'list)
          (parsed '()))
      (dolist (line lines)
        (let ((trimmed (string-trim line)))
          (unless (string-empty-p trimmed)
            (condition-case nil
                (push (json-read-from-string trimmed) parsed)
              (error
               nil)))))
      (nreverse parsed))))

(defmacro lichess-http-with-ok (binding &rest body)
  "Bind RESULT-EXPR to VAR in BINDING and execute BODY if successful.
Otherwise, return the error result.
Format: (lichess-http-with-ok (VAR RESULT-EXPR) BODY...)"
  (declare (indent 1))
  (let ((var (car binding))
        (res-sym (make-symbol "result")))
    `(let ((,res-sym ,(cadr binding)))
       (if (lichess-http-result-success ,res-sym)
           (let ((,var (lichess-http-result-data ,res-sym)))
             ,@body)
         ,res-sym))))

;;;; API Core Call Wrappers

(defun lichess-http--call-get
    (endpoint
     callback &optional headers anonymous parse-type accept-header)
  "GET JSON from ENDPOINT and call CALLBACK with a `lichess-http-result'.
Optional HEADERS is an alist of headers.
If ANONYMOUS is non-nil, the request does not include authorization.
PARSE-TYPE controls response parsing: \\='json (default) or \\='raw.
ACCEPT-HEADER specifies the Accept header (defaults to \"application/json\")."
  (lichess-http-request endpoint
                        (lambda (res-cons)
                          (let* ((status (car res-cons))
                                 (val (cdr res-cons))
                                 (res
                                  (if (and (>= status 200)
                                           (< status 300))
                                      (lichess-http-result-ok val)
                                    (lichess-http-result-err
                                     (cons status val)))))
                            (funcall callback res)))
                        :method "GET"
                        :accept (or accept-header "application/json")
                        :headers headers
                        :parse (or parse-type 'json)
                        :anonymous anonymous))

(defun lichess-http--call-post
    (endpoint params callback &optional parse-type)
  "POST to ENDPOINT with PARAMS and call CALLBACK with a `lichess-http-result'.
PARAMS is an alist of key-value parameters.
PARSE-TYPE controls response parsing: `json' (default) or `raw'."
  (lichess-http-request
   endpoint
   (lambda (res-cons)
     (let* ((status (car res-cons))
            (val (cdr res-cons))
            (res
             (if (and (>= status 200) (< status 300))
                 (lichess-http-result-ok val)
               (lichess-http-result-err (cons status val)))))
       (funcall callback res)))
   :method "POST"
   :data (and params (url-build-query-string params))
   :headers
   (and params
        '(("Content-Type" . "application/x-www-form-urlencoded")))
   :parse (or parse-type 'json)))

;;;; Stream helper

(cl-defun
 lichess-http-stream-open
 (endpoint
  &key buffer-name on-event on-open on-close method data headers)
 "Open NDJSON stream for ENDPOINT.
Use BUFFER-NAME for the network process.
ON-EVENT is a callback taking parsed JSON.
ON-OPEN is called when socket is connected.
ON-CLOSE is called when closed.
METHOD is HTTP method string.
DATA is body string.
HEADERS is alist of headers."
 (lichess-http-ndjson-open
  endpoint
  :buffer-name buffer-name
  :on-event on-event
  :on-open on-open
  :on-close on-close
  :method method
  :data data
  :headers headers))

(defun lichess-http-stream-close (stream)
  "Close STREAM returned by `lichess-http-stream-open'."
  (lichess-http-ndjson-close stream))

;;;; Declarative Endpoint Definer Macro

(defmacro lichess-http-defendpoint (name path docstring &rest keys)
  "Define an API endpoint function NAME.
PATH is the endpoint path, possibly containing `:param` placeholders.
DOCSTRING is the function documentation.
KEYS is a plist of options:
  :method       HTTP method symbol (GET or POST, default GET)
  :path-params  List of variables to replace in PATH.
  :query-params List of variables to send as query arguments.
  :post-params  List of variables to send as POST form-urlencoded fields.
  :parse-type   Parsing type: `json` (default) or `raw`.
  :accept-header Custom Accept header string."
  (declare (indent 2) (doc-string 3))
  (let*
      ((method (or (plist-get keys :method) 'GET))
       (path-params (plist-get keys :path-params))
       (query-params (plist-get keys :query-params))
       (post-params (plist-get keys :post-params))
       (parse-type (or (plist-get keys :parse-type) 'json))
       (accept-header (plist-get keys :accept-header))
       ;; Build the function argument list: path-params, query/post params, callback
       (other-args
        (append
         path-params
         (when (or query-params post-params)
           (append query-params post-params))))
       (func-args (append other-args (list 'callback))))
    `(defun ,name ,func-args
       ,docstring
       (let ((resolved-path ,path))
         ;; 1. Resolve path parameters
         ,@
         (mapcar
          (lambda (param)
            `(setq resolved-path
                   (replace-regexp-in-string
                    ,(concat ":" (symbol-name param))
                    (cond
                     ((eq ,param t)
                      "true")
                     ((eq ,param nil)
                      "false")
                     ((symbolp ,param)
                      (symbol-name ,param))
                     ((numberp ,param)
                      (number-to-string ,param))
                     (t
                      ,param))
                    resolved-path)))
          path-params)
         ;; 2. Make the HTTP call
         ,(cond
           ((eq method 'POST)
            (if post-params
                `(let* ((params
                         (list
                          ,@
                          (mapcar
                           (lambda (param)
                             `(list
                               ,(symbol-name param)
                               (cond
                                ((eq ,param t)
                                 "true")
                                ((eq ,param nil)
                                 nil)
                                ((symbolp ,param)
                                 (symbol-name ,param))
                                ((numberp ,param)
                                 (number-to-string ,param))
                                (t
                                 ,param))))
                           post-params)))
                        (filtered
                         (cl-remove-if-not
                          (lambda (x) (cadr x)) params)))
                   (lichess-http--call-post
                    resolved-path filtered callback
                    ',parse-type))
              `(lichess-http--call-post resolved-path nil callback
                                        ',parse-type)))
           (t
            (if query-params
                `(let* ((query
                         (list
                          ,@
                          (mapcar
                           (lambda (param)
                             `(list
                               ,(symbol-name param)
                               (cond
                                ((eq ,param t)
                                 "true")
                                ((eq ,param nil)
                                 nil)
                                ((symbolp ,param)
                                 (symbol-name ,param))
                                ((numberp ,param)
                                 (number-to-string ,param))
                                (t
                                 ,param))))
                           query-params)))
                        (filtered
                         (cl-remove-if-not
                          (lambda (x) (cadr x)) query))
                        (query-str
                         (and filtered
                              (url-build-query-string filtered))))
                   (lichess-http--call-get (if query-str
                                               (concat
                                                resolved-path
                                                "?"
                                                query-str)
                                             resolved-path)
                                           callback
                                           nil
                                           nil
                                           ',parse-type
                                           ,accept-header))
              `(lichess-http--call-get resolved-path callback
                                       nil
                                       nil
                                       ',parse-type
                                       ,accept-header))))))))

(defmacro lichess-http-defstream (name path docstring &rest keys)
  "Define an NDJSON stream endpoint function NAME.
PATH is the endpoint path, possibly containing `:param` placeholders.
DOCSTRING is the function documentation.
KEYS is a plist of options:
  :method       HTTP method symbol (GET or POST, default GET)
  :path-params  List of variables to replace in PATH.
  :query-params List of variables to send as query arguments.
  :post-params  List of variables to send as POST form-urlencoded fields."
  (declare (indent 2) (doc-string 3))
  (let* ((method (or (plist-get keys :method) 'GET))
         (path-params (plist-get keys :path-params))
         (query-params (plist-get keys :query-params))
         (post-params (plist-get keys :post-params))
         (other-args
          (append
           path-params
           (when (or query-params post-params)
             (append query-params post-params))))
         (func-args (append other-args (list '&rest 'stream-keys))))
    `(defun ,name ,func-args
       ,docstring
       (let ((resolved-path ,path))
         ,@
         (mapcar
          (lambda (param)
            `(setq resolved-path
                   (replace-regexp-in-string
                    ,(concat ":" (symbol-name param))
                    (cond
                     ((eq ,param t)
                      "true")
                     ((eq ,param nil)
                      "false")
                     ((symbolp ,param)
                      (symbol-name ,param))
                     ((numberp ,param)
                      (number-to-string ,param))
                     (t
                      ,param))
                    resolved-path)))
          path-params)
         (let* (,@
                (when
                 post-params
                 `((params
                    (list
                     ,@
                     (mapcar
                      (lambda (param)
                        `(list
                          ,(symbol-name param)
                          (cond
                           ((eq ,param t)
                            "true")
                           ((eq ,param nil)
                            nil)
                           ((symbolp ,param)
                            (symbol-name ,param))
                           ((numberp ,param)
                            (number-to-string ,param))
                           (t
                            ,param))))
                      post-params)))
                   (filtered
                    (cl-remove-if-not (lambda (x) (cadr x)) params))
                   (data-payload
                    (and filtered
                         (url-build-query-string filtered)))))
                ,@
                (when
                 query-params
                 `((query
                    (list
                     ,@
                     (mapcar
                      (lambda (param)
                        `(list
                          ,(symbol-name param)
                          (cond
                           ((eq ,param t)
                            "true")
                           ((eq ,param nil)
                            nil)
                           ((symbolp ,param)
                            (symbol-name ,param))
                           ((numberp ,param)
                            (number-to-string ,param))
                           (t
                            ,param))))
                      query-params)))
                   (filtered
                    (cl-remove-if-not (lambda (x) (cadr x)) query))
                   (query-str
                    (and filtered
                         (url-build-query-string filtered))))))
           (apply #'lichess-http-stream-open
                  ,(if query-params
                       `(if query-str
                            (concat resolved-path "?" query-str)
                          resolved-path)
                     'resolved-path)
                  :method ,(symbol-name method)
                  :data
                  ,(if post-params
                       'data-payload
                     'nil)
                  :headers
                  ,(if post-params
                       `(and data-payload
                             '(("Content-Type"
                                .
                                "application/x-www-form-urlencoded")))
                     'nil)
                  stream-keys))))))

(provide 'lichess-http)
;;; lichess-http.el ends here
