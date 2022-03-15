(import test)

(include "../http-client.scm")
(import http-client)

(import (chicken port))

(include "testlib.scm")

(test-begin "http-client")

;; TODO: This is messy and hard to read
(test-group "simple GET requests"
  (test-group "an empty response"
    (let* ((log (with-server-response
                 (lambda ()
                   (test "Response is EOF"
                         #!eof
                         (with-input-from-request
                          "http://example.com/some/path#more"
                          #f read-string)))
                 "HTTP/1.0 200 OK\r\nContent-Length: 0\r\n"))
           (req (log-request log)))

      (test "Request method" 'GET (request-method req))
      (test "URI is path without fragment"
            "/some/path" (uri->string (request-uri req)))
      (test "host header gets set"
            '("example.com" . #f)
            (header-value 'host (request-headers req)))
      (test "HTTP request is version 1.1"
            '(1 1)
            (list (request-major req) (request-minor req)))))

  (test-group "a response with trailing garbage"
    (with-server-response
     (lambda ()
       (test "Response excludes garbage data"
             "foo"
             (with-input-from-request
              "http://example.com" #f read-string)))
     (conc "HTTP/1.0 200 OK\r\nContent-Length: 3\r\n"
           "\r\nfoobar")))

  ;; This is (mostly) an intarweb test...
  (test-group "a short chunked response with trailing garbage"
    (with-server-response
     (lambda ()
       (test "Response is the chunked data"
             "one, two three"
             (with-input-from-request "http://example.com"
                                      #f read-string)))
     (conc "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n"
           "\r\n5\r\none, \r\n2\r\ntw\r\n7\r\no three\r\n0\r\n"
           "IGNORED TRAILING GARBAGE")))

  (test-group "400 series"
    (with-server-response
     (lambda ()
       (test-error* "404 results in client error"
                    (exn http client-error)
                    (with-input-from-request "http://example.com" #f #f)))
     (conc "HTTP/1.0 404 Not Found\r\n"))))


(test-group "request body encoding"
  (test-group "simple string body"
    (let* ((log (with-server-response
                 (lambda ()
                   (test "Response is read back"
                         "Your response, sir"
                         (with-input-from-request
                          "http://example.com" "testing" read-string)))
                 "HTTP/1.0 200 OK\r\n\r\nYour response, sir"))
           (req (log-request log)))

      (test "Request method" 'POST (request-method req))
      (test "Content type is not set"
            #f
            (header-value 'content-type (request-headers req)))
      (test "Content-length is string length"
            7 (header-value 'content-length (request-headers req)))
      (test "String was sent as body" "testing" (log-body log))))

  (test-group "string body with custom request method"
    (let* ((log (with-server-response
                 (lambda ()
                   (let* ((uri (uri-reference "http://example.com"))
                          (req (make-request uri: uri method: 'LALA)))
                     (test "Response is read back"
                           "Your response, sir"
                           (with-input-from-request
                            req "testing" read-string))))
                 "HTTP/1.0 200 OK\r\n\r\nYour response, sir"))
           (req (log-request log)))

      (test "Request method is custom" 'LALA (request-method req))
      (test "Content type is not set"
            #f
            (header-value 'content-type (request-headers req)))
      (test "Content-length is string length"
            7 (header-value 'content-length (request-headers req)))
      (test "String was sent as body" "testing" (log-body log))))

  (test-group "string body using HTTP/1.0"
    (let* ((log (with-server-response
                 (lambda ()
                   (let* ((uri (uri-reference "http://example.com"))
                          (req (make-request uri: uri method: 'LALA
                                             major: 1 minor: 0)))
                     (test "Response is read back"
                           "Your response, sir"
                           (with-input-from-request
                            req "testing" read-string))))
                 "HTTP/1.0 200 OK\r\n\r\nYour response, sir"))
           (req (log-request log)))

      (test "Request method is custom" 'LALA (request-method req))
      (test "Version is correct"
            '(1 . 0)
            (cons (request-major req) (request-minor req)))
      (test "Content type is not set"
            #f
            (header-value 'content-type (request-headers req)))
      (test "Content-length is set"
            7 (header-value 'content-length (request-headers req)))
      (test "String was sent as body" "testing" (log-body log))))

  (test-group "alist form data body"
    (let* ((log (with-server-response
                 (lambda ()
                   (with-input-from-request
                    "http://example.com"
                    '((lala . "testing")
                      (another . "data")
                      ("more" . stuff))
                    read-string))
                 "HTTP/1.0 200 OK\r\n\r\n"))
           (req (log-request log)))

      (test "Request method" 'POST (request-method req))
      (test "Content type is form encoding"
            'application/x-www-form-urlencoded
            (header-value 'content-type (request-headers req)))
      (test "Content-length was set correctly"
            36 (header-value 'content-length (request-headers req)))
      (test "Body was sent correctly"
            "lala=testing&another=data&more=stuff" (log-body log))))

  (test-group "alist form data body with string port"
    (let* ((string-port (open-input-string "the file's contents"))
           (log (with-server-response
                 (lambda ()
                   (with-input-from-request
                    "http://example.com"
                    `((lala . "testing")
                      (the-file file: ,string-port
                                filename: "str")
                      ("more" . stuff))
                    read-string))
                 "HTTP/1.0 200 OK\r\n\r\n"))
           (req (log-request log))
           (h (request-headers req))
           (boundary (header-param 'boundary 'content-type h))
           (expected-data
            (conc
             "--" boundary "\r\n"
             "Content-Disposition: form-data; name=\"lala\"\r\n\r\n"
             "testing\r\n"
             "--" boundary "\r\n"
             "Content-Disposition: form-data; name=\"the-file\"; "
             "filename=\"str\"\r\n"
             "Content-Type: application/octet-stream\r\n\r\n"
             "the file's contents\r\n"
             "--" boundary "\r\n"
             "Content-Disposition: form-data; name=\"more\"\r\n\r\n"
             "stuff\r\n"
             "--" boundary "--\r\n")))

      (test "Request method" 'POST (request-method req))
      (test "Content type is multipart"
            'multipart/form-data
            (header-value 'content-type h))
      (test "Content-length was set"
            504 (header-value 'content-length h))
      (test "Version is the default HTTP version of 1.1"
            '(1 . 1)
            (cons (request-major req) (request-minor req)))
      (test "Transfer encoding is not set"
            #f
            (header-value 'transfer-encoding (request-headers req)))
      (test "Body contains the file and other data, delimited by the boundary"
            expected-data (log-body log))))

  (test-group "alist form data body with string port using HTTP/1.0"
    (let* ((string-port (open-input-string "the file's contents"))
           (uri (uri-reference "http://example.com"))
           (req (make-request uri: uri method: 'POST
                              major: 1 minor: 0))
           (log (with-server-response
                 (lambda ()
                   (with-input-from-request
                    req
                    `((lala . "testing")
                      (the-file file: ,string-port
                                filename: "str")
                      ("more" . stuff))
                    read-string))
                 "HTTP/1.0 200 OK\r\n\r\n"))
           (req (log-request log))
           (h (request-headers req))
           (boundary (header-param 'boundary 'content-type h))
           (expected-data
            (conc
             "--" boundary "\r\n"
             "Content-Disposition: form-data; name=\"lala\"\r\n\r\n"
             "testing\r\n"
             "--" boundary "\r\n"
             "Content-Disposition: form-data; name=\"the-file\"; "
             "filename=\"str\"\r\n"
             "Content-Type: application/octet-stream\r\n\r\n"
             "the file's contents\r\n"
             "--" boundary "\r\n"
             "Content-Disposition: form-data; name=\"more\"\r\n\r\n"
             "stuff\r\n"
             "--" boundary "--\r\n")))

      (test "Request method" 'POST (request-method req))
      (test "Content type is multipart"
            'multipart/form-data
            (header-value 'content-type h))
      (test "Content-length was set"
            504 (header-value 'content-length h))
      (test "Version is correct"
            '(1 . 0)
            (cons (request-major req) (request-minor req)))
      (test "Transfer encoding is not set"
            #f
            (header-value 'transfer-encoding (request-headers req)))
      (test "Body contains the file and other data, delimited by the boundary"
            expected-data (log-body log))))

  (test-group "alist form data body with custom port"
    (let* ((string-port (open-input-string "the file's contents"))
           (custom-port (make-input-port (lambda () (read-char string-port)) (constantly #t) (lambda () (close-input-port string-port))))
           (log (with-server-response
                 (lambda ()
                   (with-input-from-request
                    "http://example.com"
                    `((lala . "testing")
                      (the-file file: ,custom-port
                                filename: "str")
                      ("more" . stuff))
                    read-string))
                 "HTTP/1.0 200 OK\r\n\r\n"))
           (req (log-request log))
           (h (request-headers req))
           (boundary (header-param 'boundary 'content-type h))
           (expected-data
            (conc
             "--" boundary "\r\n"
             "Content-Disposition: form-data; name=\"lala\"\r\n\r\n"
             "testing\r\n"
             "--" boundary "\r\n"
             "Content-Disposition: form-data; name=\"the-file\"; "
             "filename=\"str\"\r\n"
             "Content-Type: application/octet-stream\r\n\r\n"
             "the file's contents\r\n"
             "--" boundary "\r\n"
             "Content-Disposition: form-data; name=\"more\"\r\n\r\n"
             "stuff\r\n"
             "--" boundary "--\r\n")))

      (test "Request method" 'POST (request-method req))
      (test "Content type is multipart"
            'multipart/form-data
            (header-value 'content-type h))
      (test "Content-length was not set"
            #f (header-value 'content-length h))
      (test "Version is the default HTTP version of 1.1"
            '(1 . 1)
            (cons (request-major req) (request-minor req)))
      (test "Transfer encoding is chunked"
            'chunked
            (header-value 'transfer-encoding (request-headers req)))
      (test "Body contains the file and other data, delimited by the boundary"
            expected-data (log-body log))))

  (test-group "alist form data body with custom port using HTTP/1.0"
    (let* ((string-port (open-input-string "the file's contents"))
           (custom-port (make-input-port (lambda () (read-char string-port)) (constantly #t) (lambda () (close-input-port string-port))))
           (uri (uri-reference "http://example.com"))
           (req (make-request uri: uri method: 'POST
                              major: 1 minor: 0))
           (log (with-server-response
                 (lambda ()
                   (with-input-from-request
                    req
                    `((lala . "testing")
                      (the-file file: ,custom-port
                                filename: "str")
                      ("more" . stuff))
                    read-string))
                 "HTTP/1.0 200 OK\r\n\r\n"))
           (req (log-request log))
           (h (request-headers req))
           (boundary (header-param 'boundary 'content-type h))
           (expected-data
            (conc
             "--" boundary "\r\n"
             "Content-Disposition: form-data; name=\"lala\"\r\n\r\n"
             "testing\r\n"
             "--" boundary "\r\n"
             "Content-Disposition: form-data; name=\"the-file\"; "
             "filename=\"str\"\r\n"
             "Content-Type: application/octet-stream\r\n\r\n"
             "the file's contents\r\n"
             "--" boundary "\r\n"
             "Content-Disposition: form-data; name=\"more\"\r\n\r\n"
             "stuff\r\n"
             "--" boundary "--\r\n")))

      (test "Request method" 'POST (request-method req))
      (test "Content type is multipart"
            'multipart/form-data
            (header-value 'content-type h))
      (test "Content-length was not set"
            #f (header-value 'content-length h))
      (test "Version is correct"
            '(1 . 0)
            (cons (request-major req) (request-minor req)))
      (test "Transfer encoding is not set"
            #f
            (header-value 'transfer-encoding (request-headers req)))
      (test "Body contains the file and other data, delimited by the boundary"
            expected-data (log-body log))))

  (test-group "alist form data body with filename"
    (let* ((tmpfile (create-temporary-file))
           (log (with-server-response
                 (lambda ()
                   (with-output-to-file tmpfile
                     (lambda () (display "the file's contents")))
                   (with-input-from-request
                    "http://example.com"
                    `((lala . "testing")
                      (the-file file: ,tmpfile filename: "tmpfile")
                      ("more" . stuff))
                    read-string))
                 "HTTP/1.0 200 OK\r\n\r\n"))
           (req (log-request log))
           (h (request-headers req))
           (boundary (header-param 'boundary 'content-type h))
           (expected-data
            (conc
             "--" boundary "\r\n"
             "Content-Disposition: form-data; name=\"lala\"\r\n\r\n"
             "testing\r\n"
             "--" boundary "\r\n"
             "Content-Disposition: form-data; name=\"the-file\"; "
             "filename=\"tmpfile\"\r\n"
             "Content-Type: application/octet-stream\r\n\r\n"
             "the file's contents\r\n"
             "--" boundary "\r\n"
             "Content-Disposition: form-data; name=\"more\"\r\n\r\n"
             "stuff\r\n"
             "--" boundary "--\r\n")))

      (test "Request method" 'POST (request-method req))
      (test "Content type is multipart"
            'multipart/form-data
            (header-value 'content-type h))
      (test "Content-length was set to the entire body size"
            (string-length expected-data)
            (header-value 'content-length h))
      (test "Body contains the file and other data, delimited by the boundary"
            expected-data (log-body log))))

  (test-group "custom writer procedure"
    (let* ((log (with-server-response
                 (lambda ()
                   (test "Response is read back"
                         "Your response, sir"
                         (with-input-from-request
                          "http://example.com"
                          (lambda ()
                            (display "test, ")
                            (display "test, 123"))
                          read-string)))
                 "HTTP/1.0 200 OK\r\n\r\nYour response, sir"))
           (req (log-request log)))

      (test "Request method" 'POST (request-method req))
      (test "Content type is not set"
            #f
            (header-value 'content-type (request-headers req)))
      (test "Transfer encoding is chunked"
            'chunked
            (header-value 'transfer-encoding (request-headers req)))
      (test "Content-length is not set"
            #f (header-value 'content-length (request-headers req)))
      (test "All writes were received"
            "test, test, 123" (log-body log))))

  (test-group "custom writer procedure with content-length header"
    (let* ((req (make-request uri: (uri-reference "http://example.com")
                              headers: (headers `((content-length 15)))
                              method: 'POST))
           (log (with-server-response
                 (lambda ()
                   (test "Response is read back"
                         "Your response, sir"
                         (with-input-from-request
                          req
                          (lambda ()
                            (display "test, ")
                            (display "test, 123"))
                          read-string)))
                 "HTTP/1.0 200 OK\r\n\r\nYour response, sir"))
           (req (log-request log)))

      (test "Request method" 'POST (request-method req))
      (test "Content type is not set"
            #f
            (header-value 'content-type (request-headers req)))
      (test "Transfer encoding is not set"
            #f
            (header-value 'transfer-encoding (request-headers req)))
      (test "Content-length is taken from user-supplied header"
            15 (header-value 'content-length (request-headers req)))
      (test "All writes were received"
            "test, test, 123" (log-body log))))

  (test-group "custom writer procedure with http/1.0 and no content-length"
    (let* ((req (make-request uri: (uri-reference "http://example.com")
                              method: 'POST major: 1 minor: 0))
           (log (with-server-response
                 (lambda ()
                   (test "Response is read back"
                         "Your response, sir"
                         (with-input-from-request
                          req
                          (lambda ()
                            (display "test, ")
                            (display "test, 123"))
                          read-string)))
                 "HTTP/1.0 200 OK\r\n\r\nYour response, sir"))
           (req (log-request log)))

      (test "Request method" 'POST (request-method req))
      (test "Content type is not set"
            #f
            (header-value 'content-type (request-headers req)))
      (test "Transfer encoding is not set"
            #f
            (header-value 'transfer-encoding (request-headers req)))
      (test "Content-length is not set"
            #f (header-value 'content-length (request-headers req)))
      ;; We could set connection: close, but for HTTP/1.0 that doesn't
      ;; really exist
      (test "Connection is not set"
            #f (header-value 'connection (request-headers req)))
      (test "All writes were received"
            "test, test, 123" (log-body log)))))

(test-group "Redirects"
  (test-group "single permanent GET redirect"
    (let* ((logs (with-server-responses
                  (lambda ()
                    (test "Final response matches final request"
                          "Got here"
                          (with-input-from-request
                           "http://example.com/some/path#more"
                           #f read-string)))
                  (conc "HTTP/1.0 301 Moved Permanently\r\n"
                        "Location: http://example.org/different/path\r\n"
                        "Content-Length: 8\r\n\r\nIgnored!")
                  (conc "HTTP/1.0 200 OK\r\nContent-Length: 8\r\n\r\n"
                        "Got here")))
           (req1 (log-request (car logs)))
           (req2 (log-request (cadr logs))))

      (test "Redirected URI is new path"
            "/different/path" (uri->string (request-uri req2)))
      (test "host header gets set on second request"
            '("example.org" . #f)
            (header-value 'host (request-headers req2)))
      (test "HTTP request is version 1.1 (even though response was 1.0)"
            '(1 1)
            (list (request-major req2) (request-minor req2)))))

  (test-group "single permanent POST redirect"
    (let* ((logs (with-server-responses
                  (lambda ()
                    (test "Final response matches final request"
                          "Got here"
                          (with-input-from-request
                           "http://example.com/some/path#more"
                           '((foo . "bar")) read-string)))
                  (conc "HTTP/1.0 301 Moved Permanently\r\n"
                        "Location: http://example.org/different/path\r\n"
                        "Content-Length: 8\r\n\r\nIgnored!")
                  (conc "HTTP/1.0 200 OK\r\nContent-Length: 8\r\n\r\n"
                        "Got here")))
           (req1 (log-request (car logs)))
           (req2 (log-request (cadr logs))))

      (test "Redirected URI is new path"
            "/different/path" (uri->string (request-uri req2)))
      (test "HTTP method is still POST" 'POST (request-method req2))
      (test "Correct content-length on both requests"
            '(7 7)
            (list (header-value 'content-length (request-headers req1))
                  (header-value 'content-length (request-headers req2))))
      (test "Body got sent to target" "foo=bar" (log-body (cadr logs)))))

  (test-group "single \"see other\" POST redirect"
    (let* ((logs (with-server-responses
                  (lambda ()
                    (test "Final response matches final request"
                          "Got here"
                          (with-input-from-request
                           "http://example.com/some/path#more"
                           '((foo . "bar")) read-string)))
                  (conc "HTTP/1.0 303 See Other\r\n"
                        "Location: http://example.org/different/path\r\n"
                        "Content-Length: 8\r\n\r\nIgnored!")
                  (conc "HTTP/1.0 200 OK\r\nContent-Length: 8\r\n\r\n"
                        "Got here")))
           (req1 (log-request (car logs)))
           (req2 (log-request (cadr logs))))

      (test "Redirected URI is new path"
            "/different/path" (uri->string (request-uri req2)))
      (test "HTTP method switched to GET" 'GET (request-method req2))
      (test "Zero content-length on target"
            0
            (header-value 'content-length (request-headers req2)))
      (test "No body got sent to target" "" (log-body (cadr logs)))))

  (test-group "Multiple redirects, just below maximum"
    (parameterize ((max-redirect-depth 3))
      (let* ((logs (with-server-responses
                    (lambda ()
                      (test "Final response matches final request"
                            "Got here"
                            (with-input-from-request
                             "http://example.com/some/path#more"
                             #f read-string)))
                    (conc "HTTP/1.0 301 Moved Permanently\r\n"
                          "Location: http://example.org/different/path\r\n"
                          "Content-Length: 8\r\n\r\nIgnored!")
                    (conc "HTTP/1.0 301 Moved Permanently\r\n"
                          "Location: http://example.org/new/path\r\n"
                          "Content-Length: 8\r\n\r\nIgnored!")
                    (conc "HTTP/1.0 301 Moved Permanently\r\n"
                          "Location: http://example.net/newer/path\r\n"
                          "Content-Length: 8\r\n\r\nIgnored!")
                    (conc "HTTP/1.0 200 OK\r\nContent-Length: 8\r\n\r\n"
                          "Got here")))
             (req (log-request (last logs))))

        (test "Redirected URI is new path"
              "/newer/path" (uri->string (request-uri req)))
        (test "host header gets set on last request"
              '("example.net" . #f)
              (header-value 'host (request-headers req)))
        (test "HTTP request is still version 1.1"
              '(1 1) (list (request-major req) (request-minor req))))))

  (test-group "exceeding maximum redirects"
    (parameterize ((max-redirect-depth 2))
      (test-error* "results in a client redirect error"
                   (exn http redirect-depth-exceeded)
                   (with-server-responses
                    (lambda ()
                      (with-input-from-request
                       "http://example.com" #f read-string))
                    (conc "HTTP/1.0 301 Moved Permanently\r\n"
                          "Location: http://example.org/different/path\r\n"
                          "Content-Length: 8\r\n\r\nIgnored!")
                    (conc "HTTP/1.0 301 Moved Permanently\r\n"
                          "Location: http://example.org/new/path\r\n"
                          "Content-Length: 8\r\n\r\nIgnored!")
                    (conc "HTTP/1.0 301 Moved Permanently\r\n"
                          "Location: http://example.net/newer/path\r\n"
                          "Content-Length: 8\r\n\r\nIgnored!")
                    (conc "HTTP/1.0 200 OK\r\nContent-Length: 19\r\n\r\n"
                          "Should not get here"))))))

(test-group "Retries"
  (test-group "premature disconnect by server"
    (test-group "just below maximum retries"
      (parameterize ((max-retry-attempts 3))
        (let* ((logs (with-server-responses
                      (lambda ()
                        (test "Final response matches final request"
                              "It worked at last"
                              (with-input-from-request
                               "http://example.com/blah" #f read-string)))
                      ;; Empty responses
                      "" ;; 0 retries
                      "" ;; 1 retry
                      "" ;; 2 retries
                      (conc "HTTP/1.0 200 OK\r\n"
                            "Content-Length: 17\r\n\r\n"
                            "It worked at last")))
               (req (log-request (last logs))))

          ;; Just a few random checks
          (test "URI is still OK"
                "/blah" (uri->string (request-uri req)))
          (test "host header is also OK"
                '("example.com" . #f)
                (header-value 'host (request-headers req)))
          (test "HTTP request is version 1.1"
                '(1 1) (list (request-major req) (request-minor req)))
          (test "No body got sent (GET)" #f (log-body (last logs))))))

    (test-group "exceeding maximum retries"
      (parameterize ((max-retry-attempts 3))
        (test-error* "results in a premature disconnection error"
                     (exn http premature-disconnection)
                     (with-server-responses
                      (lambda ()
                        (with-input-from-request
                         "http://example.com/" #f read-string))
                      ;; Empty responses
                      ""     ;; 0 retries
                      ""     ;; 1 retry
                      ""     ;; 2 retries
                      "")))) ;; 3 retries

    (test-group "no retries when retry-request? returns #f"
      (parameterize ((max-retry-attempts 5)
                     (retry-request? (lambda (r) #f)))
        (test-error* "results in a premature-disconnection error"
                     (exn http premature-disconnection)
                     (with-server-responses
                      (lambda ()
                        (with-input-from-request
                         "http://foo:bar@example.com/" #f read-string))
                      ;; Empty responses
                      ""      ;; 0 retries
                      ""))))) ;; 1 retry

  (test-group "unauthorized"
    (test-group "just below maximum retries"
      (parameterize ((max-retry-attempts 2))
        (let* ((logs (with-server-responses
                      (lambda ()
                        (test "Final response is ok"
                              "You got the password right"
                              (with-input-from-request
                               "http://foo:bar@example.com/blah" #f read-string)))
                      (conc "HTTP/1.0 401 Unauthorized\r\n"
                            "WWW-Authenticate: basic realm=\"x\"\r\n"
                            "Content-Length: 7\r\n\r\n"
                            "Retry 0")
                      (conc "HTTP/1.0 401 Unauthorized\r\n"
                            "WWW-Authenticate: basic realm=\"x\"\r\n"
                            "Content-Length: 7\r\n\r\n"
                            "Retry 1")
                      (conc "HTTP/1.0 200 OK\r\n"
                            "Content-Length: 26\r\n\r\n"
                            "You got the password right")))
               (req (log-request (last logs))))

          ;; Just a few random checks
          (test "URI is still OK"
                "/blah" (uri->string (request-uri req)))
          (test "host header is also OK"
                '("example.com" . #f)
                (header-value 'host (request-headers req)))
          (test "HTTP request is version 1.1"
                '(1 1) (list (request-major req) (request-minor req)))
          (test "No body got sent (GET)" #f (log-body (last logs))))))

    (test-group "exceeding maximum retries"
      (parameterize ((max-retry-attempts 2))
        (test-error* "results in a client error"
                     (exn http client-error)
                     (with-server-responses
                      (lambda ()
                        (with-input-from-request
                         "http://foo:bar@example.com/" #f read-string))
                      (conc "HTTP/1.0 401 Unauthorized\r\n"
                            "WWW-Authenticate: basic realm=\"x\"\r\n"
                            "Content-Length: 7\r\n\r\n"
                            "Retry 0")
                      (conc "HTTP/1.0 401 Unauthorized\r\n"
                            "WWW-Authenticate: basic realm=\"x\"\r\n"
                            "Content-Length: 7\r\n\r\n"
                            "Retry 1")
                      (conc "HTTP/1.0 401 Unauthorized\r\n"
                            "WWW-Authenticate: basic realm=\"x\"\r\n"
                            "Content-Length: 7\r\n\r\n"
                            "Retry 2")))))

    ;; TODO: Figure out some way to test the retries when there's a
    ;; net i/o error.

    (test-group "retries are OK for unauthorized when retry-request? returns #f"
      (parameterize ((max-retry-attempts 5)
                     (retry-request? (lambda (r) #f)))
        (let* ((logs (with-server-responses
                      (lambda ()
                        (test "Final response is ok"
                              "You got the password right"
                              (with-input-from-request
                               "http://foo:bar@example.com/blah" #f read-string)))
                      (conc "HTTP/1.0 401 Unauthorized\r\n"
                            "WWW-Authenticate: basic realm=\"x\"\r\n"
                            "Content-Length: 7\r\n\r\n"
                            "Retry 0")
                      (conc "HTTP/1.0 200 OK\r\n"
                            "Content-Length: 26\r\n\r\n"
                            "You got the password right")))
               (req (log-request (last logs))))

          (test "URI is still OK"
                "/blah" (uri->string (request-uri req)))
          (test "host header is also OK"
                '("example.com" . #f)
                (header-value 'host (request-headers req)))
          (test "HTTP request is version 1.1"
                '(1 1) (list (request-major req) (request-minor req)))
          (test "No body got sent (GET)" #f (log-body (last logs))))))))

(test-group "url normalization"
  (let* ((logs (with-server-responses
                (lambda ()
                  ;; Reported by Caolan McMahon in #1448: URI paths
                  ;; would be re-encoded in a lossy way, dropping
                  ;; special characters.
                  (with-input-from-request
                   "https://img.discogs.com/dMvk8q681FkVCkhv3qRvTfwlLZk=/fit-in/300x300/filters:strip_icc():format(jpeg):mode_rgb():quality(40)/discogs-images/R-8062430-1454420247-1268.jpeg.jpg" #f read-string))
                (conc "HTTP/1.0 200 OK\r\n\r\n")))
         (req (log-request (last logs))))
    (test "URI path was not mangled"
          "/dMvk8q681FkVCkhv3qRvTfwlLZk=/fit-in/300x300/filters:strip_icc():format(jpeg):mode_rgb():quality(40)/discogs-images/R-8062430-1454420247-1268.jpeg.jpg"
          (uri->string (request-uri req)))))

(test-group "error handling"
  (with-server-responses
   (lambda ()
     (test-error* "Invalid uri"
                  (exn http bad-uri)
                  (with-input-from-request "%" #f read-string))))
  ;; TODO: Why shouldn't empty POST be allowed?
  (with-server-responses
   (lambda ()
     (test-error* "Invalid form data"
                  (exn http form-data-error)
                  (with-input-from-request
                   "http://example.com" '() read-string)))))


(test-end "http-client")

(test-exit)
