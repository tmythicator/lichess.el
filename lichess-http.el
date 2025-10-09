;;; lichess-http.el --- HTTP/JSON/NDJSON helpers for Lichess -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025  Alexandr Timchenko
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

;;;; Header helpers
(defun lichess-http--auth-header-line ()
  "Return raw Authorization header line for manual sockets, or \"\"."
  (if (and (boundp 'lichess-token)
           (stringp lichess-token)
           (> (length lichess-token) 0))
      (format "Authorization: Bearer %s\r\n" lichess-token)
    ""))

(defun lichess-http--auth-headers (&optional extra accept)
  "Return an alist of headers. Adds Authorization when `lichess-token' is set.
EXTRA (alist) is appended. ACCEPT, when non-nil, sets Accept header."
  (append
   (when (and (boundp 'lichess-token)
              (stringp lichess-token)
              (> (length lichess-token) 0))
     `(("Authorization" . ,(concat "Bearer " lichess-token))))
   (when accept `(("Accept" . ,accept)))
   extra))

(defun lichess-http--abs-url (url-or-endpoint)
  "Return absolute URL for URL-OR-ENDPOINT (prepend https://lichess.org if needed)."
  (if (string-prefix-p "http" url-or-endpoint)
      url-or-endpoint
    (concat "https://lichess.org" url-or-endpoint)))

;;;; Core async request
(defun lichess-http-request (url-or-endpoint callback
                                             &rest plist)
  "Perform an async HTTP request and call CALLBACK with a cons (STATUS . JSON/STRING/NIL).

URL-OR-ENDPOINT can be absolute or something like \"/api/tv/channels\".

PLIST keys:
  :method   HTTP method string (default \"GET\")
  :headers  Alist of headers to add (Authorization is auto if `lichess-token' set)
  :accept   Accept header (e.g., \"application/json\")
  :data     Request body string (UTF-8)
  :parse    One of: 'json (default), 'raw (return raw body string)

CALLBACK receives (STATUS . VALUE), where VALUE is:
  - parsed JSON object when :parse 'json and parsing succeeds
  - nil if JSON parsing fails
  - raw string when :parse 'raw."
  (let* ((method (or (plist-get plist :method) "GET"))
         (headers (plist-get plist :headers))
         (accept  (plist-get plist :accept))
         (data    (plist-get plist :data))
         (parse   (or (plist-get plist :parse) 'json))
         (url-request-method method)
         (url-request-extra-headers (lichess-http--auth-headers headers accept))
         (url-request-data (when data (encode-coding-string data 'utf-8)))
         (abs (lichess-http--abs-url url-or-endpoint)))
    (url-retrieve
     abs
     (lambda (_)
       (let* ((status (or (bound-and-true-p url-http-response-status) 0)))
         (goto-char (or url-http-end-of-headers (point-min)))
         (pcase parse
           ('raw
            (let ((body (buffer-substring-no-properties (point) (point-max))))
              (funcall callback (cons status body))))
           (_
            (let* ((body (buffer-substring-no-properties (point) (point-max)))
                   (json (condition-case _
                             (let ((json-object-type 'alist)
                                   (json-array-type 'list))
                               (json-read-from-string body))
                           (error nil))))
              (funcall callback (cons status json))))))
       ;; url lib leaves the temp buffer current; kill it
       (when (buffer-live-p (current-buffer))
         (kill-buffer (current-buffer)))))
    nil t))

(defun lichess-http-json (url-or-endpoint callback &optional headers)
  "GET JSON from URL-OR-ENDPOINT and call CALLBACK with (STATUS . JSON-or-nil).
HEADERS is an alist to add; Authorization is added automatically if available."
  (lichess-http-request url-or-endpoint callback
                        :method "GET"
                        :accept "application/json"
                        :headers headers
                        :parse 'json))

;;;; NDJSON streaming (manual TLS socket)
(cl-defstruct lichess-http-stream
  proc buf seen-headers chunk-tail)

(defun lichess-http--chunk-size-line-p (line)
  "Non-nil if LINE looks like an HTTP/1.1 chunk-size marker."
  (or (string-match-p "\\`[0-9A-Fa-f]+\\(?:;.*\\)?\\'" line)
      (string-match-p "\\`[0-9]+\\'" line)))

(cl-defun lichess-http-ndjson-open (url-or-endpoint &key
                                                    buffer-name
                                                    on-event    ;; (lambda (obj))
                                                    on-open     ;; (lambda (proc buf))
                                                    on-close)   ;; (lambda (proc msg))
  "Open an NDJSON stream to URL-OR-ENDPOINT and return a `lichess-http-stream' struct.

Arguments:
  BUFFER-NAME  Name for the process buffer (created if missing).
  ON-EVENT     Called with one parsed JSON object per line.
  ON-OPEN      Called once when the socket is connected.
  ON-CLOSE     Called when the process terminates; receives (PROC MSG)."
  (let* ((abs (lichess-http--abs-url url-or-endpoint))
         (buf (get-buffer-create (or buffer-name "*Lichess NDJSON*")))
         (seen-headers nil)
         (tail "")
         (proc (open-network-stream
                (format "lichess-ndjson-%x" (random))
                buf "lichess.org" 443 :type 'tls)))
    (with-current-buffer buf (special-mode))
    (set-process-filter
     proc
     (lambda (_proc chunk)
       ;; Accumulate and strip headers once
       (setq tail (concat tail chunk))
       (unless seen-headers
         (let ((hdr-end (string-match "\r?\n\r?\n" tail)))
           (when hdr-end
             (setq seen-headers t)
             (setq tail (substring tail (+ hdr-end (length (match-string 0 tail))))))))
       (when seen-headers
         (let ((lines (split-string tail "\n")))
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
                            (obj (json-read-from-string line)))
                       (when (functionp on-event) (funcall on-event obj)))
                   (error
                    ;; ignore malformed line; streams can be mid-chunk
                    nil))))))
           (setq tail (car (last lines)))))))
    (set-process-sentinel
     proc
     (lambda (p msg)
       (when (functionp on-close) (funcall on-close p (string-trim msg)))))
    ;; Send HTTP request
    (let* ((path (if (string-prefix-p "http" url-or-endpoint)
                     ;; extract path from absolute URL
                     (let ((u (url-generic-parse-url url-or-endpoint)))
                       (concat (or (url-filename u) "/")
                               (let ((q (url-target u))) (or q ""))))
                   url-or-endpoint)))
      (process-send-string
       proc
       (concat
        (format "GET %s HTTP/1.1\r\n" path)
        "Host: lichess.org\r\n"
        "User-Agent: Emacs\r\n"
        "Accept: application/x-ndjson\r\n"
        (lichess-http--auth-header-line)
        "Connection: keep-alive\r\n\r\n")))
    (when (functionp on-open) (funcall on-open proc buf))
    (make-lichess-http-stream :proc proc :buf buf :seen-headers seen-headers :chunk-tail tail)))

(defun lichess-http-ndjson-close (stream)
  "Close STREAM returned by `lichess-http-ndjson-open'."
  (when (and stream (process-live-p (lichess-http-stream-proc stream)))
    (delete-process (lichess-http-stream-proc stream))))

(provide 'lichess-http)
;;; lichess-http.el ends here
