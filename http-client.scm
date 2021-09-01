;;;
;;; Convenient HTTP client library
;;;
;; Copyright (c) 2008-2021, Peter Bex
;; Parts copyright (c) 2000-2004, Felix L. Winkelmann
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above
;;    copyright notice, this list of conditions and the following
;;    disclaimer in the documentation and/or other materials provided
;;    with the distribution.
;; 3. Neither the name of the author nor the names of its
;;    contributors may be used to endorse or promote products derived
;;    from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.
;
(module http-client
    (max-retry-attempts max-redirect-depth max-idle-connections
     retry-request? client-software
     close-connection! close-idle-connections!
     call-with-input-request call-with-input-request*
     with-input-from-request call-with-response
     store-cookie! delete-cookie! get-cookies-for-uri
     http-authenticators get-username/password
     basic-authenticator digest-authenticator
     determine-username/password determine-proxy
     determine-proxy-from-environment determine-proxy-username/password
     server-connector default-server-connector
     prepare-request default-prepare-request)

(import scheme
        srfi-1 srfi-13 srfi-18 srfi-69
        (chicken base) (chicken string) (chicken time)
        (chicken sort) (chicken io) (chicken file posix) (chicken format)
        (chicken process-context) (chicken process-context posix)
        (chicken port) (chicken file) (chicken tcp) (chicken condition)
        (chicken pathname)
        intarweb uri-common simple-md5 sendfile)

;; Major TODOs:
;; * Find a better approach for storing cookies, which does not
;;    lead to memory leaks.
;; * Implement md5-sess handling for digest auth
;; * Use nonce count in digest auth (is this even needed? I think it's only
;;    needed if there are webservers out there that send the same nonce
;;    repeatedly. This client doesn't do request pipelining so we don't
;;    generate requests with the same nonce if the server doesn't)
;; * Test and document SSL support
;; * The authenticators stuff is really really ugly.  It's intentionally
;;    undocumented so nobody is going to rely on it too much yet, and
;;    we have the freedom to change it later.

(define-record http-connection base-uri inport outport proxy)

(define max-retry-attempts (make-parameter 1))
(define max-redirect-depth (make-parameter 5))
;; Total idle connections.  Maybe later we'll add per-server limits.
(define max-idle-connections (make-parameter 32))

(define retry-request? (make-parameter idempotent?))

(define (determine-proxy-from-environment uri)
  (let* ((is-cgi-process (get-environment-variable "REQUEST_METHOD"))
         ;; If we're running in a CGI script, don't use HTTP_PROXY, to
         ;; avoid a "httpoxy" attack.  Instead, we use the variable
         ;; CGI_HTTP_PROXY.  See https://httpoxy.org
         (proxy-variable
          (if (and (eq? (uri-scheme uri) 'http) is-cgi-process)
              "cgi_http_proxy"
              (conc (uri-scheme uri) "_proxy")))
         (no-proxy (or (get-environment-variable "no_proxy")
                       (get-environment-variable "NO_PROXY")))
         (no-proxy (and no-proxy (map (lambda (s)
                                        (string-split s ":"))
                                      (string-split no-proxy ","))))
         (host-excluded? (lambda (entry)
                           (let ((host (car entry))
                                 (port (and (pair? (cdr entry))
                                            (string->number (cadr entry)))))
                             (and (or (string=? host "*")
                                      (string-ci=? host (uri-host uri)))
                                  (or (not port)
                                      (= (uri-port uri) port)))))))
    (cond
     ((and no-proxy (any host-excluded? no-proxy)) #f)
     ((or (get-environment-variable proxy-variable)
          (get-environment-variable (string-upcase proxy-variable))
          (get-environment-variable "all_proxy")
          (get-environment-variable "ALL_PROXY")) =>
          (lambda (proxy)               ; TODO: make this just absolute-uri
            (and-let* ((proxy-uri (uri-reference proxy))
                       ((absolute-uri? proxy-uri)))
              proxy-uri)))
     (else #f))))

(define determine-proxy (make-parameter determine-proxy-from-environment))

(define determine-proxy-username/password
  (make-parameter (lambda (uri realm)
                    (values (uri-username uri) (uri-password uri)))))

;; Maybe only pass uri and realm to this?
(define determine-username/password
  (make-parameter (lambda (uri realm)
                    (values (uri-username uri) (uri-password uri)))))

(define client-software
  (make-parameter (list (list "http-client" "1.2" "CHICKEN Scheme HTTP-client"))))


(define (with-mutex m thunk)
  (dynamic-wind
      (lambda () (mutex-lock! m))
      thunk
      (lambda () (mutex-unlock! m))))

;; TODO: find a smarter storage mechanism.  Also, this implementation
;; means cookies are shared between threads, which might not (always)
;; be desirable.
(define *cookie-jar* (list))

;; A hash table containing uri-host&port as keys.  Values are circular
;; lists of connections, pointing to the cons cell of the oldest one.
(define *idle-connections*
  (make-hash-table
   (lambda (a b)
     (and (equal? (uri-port a) (uri-port b))
          (equal? (uri-host a) (uri-host b))))
   (lambda (uri . maybe-bound)
     (apply string-hash
            (sprintf "~S ~S" (uri-host uri) (uri-port uri))
            maybe-bound))))

;; This mutex also stores the connection count.  However, it should be
;; locked whenever *idle-connections* is accessed, because another
;; thread should not be able to claim a connection we consider using.
(define *idle-connections-mutex* (make-mutex '*idle-connections*))
(mutex-specific-set! *idle-connections-mutex* 0)

(define (connection-dropped? con)
  (or (port-closed? (http-connection-inport con))
      (port-closed? (http-connection-outport con))
      (condition-case
          (and (char-ready? (http-connection-inport con))
               (eof-object? (peek-char (http-connection-inport con))))
        ;; Assume connection got reset when we get this exception
        ((exn i/o net) #t))))

;; Remove the first (oldest) idle connection which is still alive from
;; the pool and return it.  Any dead connections are pruned.
(define (grab-idle-connection! uri)
  (with-mutex
   *idle-connections-mutex*
   (lambda ()
     (define (take-first-idle-connection!)
       ;; This picks the first idle connection, if any, and removes it
       ;; from the list.
       (and-let* ((head (hash-table-ref/default *idle-connections* uri #f))
                  (connection (car head))
                  (next (cdr head))
                  (count (mutex-specific *idle-connections-mutex*)))
         (if (eq? next head)
             (hash-table-delete! *idle-connections* uri)
             ;; Rip out the next entry and move its value forward
             (begin (set-car! head (car next))
                    (set-cdr! head (cdr next))))
         (mutex-specific-set! *idle-connections-mutex* (sub1 count))
         connection))
     
     (let lp ()
       (and-let* ((con (take-first-idle-connection!)))
         (cond ((connection-dropped? con)
                (close-connection! con)
                (lp))
               (else con)))))))

;; If max-idle-connections is not yet reached, add it to the pool.  We
;; add it to the end because it is the freshest one.  This ensures
;; we'll re-use the oldest first, trying to keep them all alive.  If
;; the maximum is reached, close and discard the connection.
(define (maybe-add-idle-connection! uri con)
  (with-mutex
   *idle-connections-mutex*
   (lambda ()
     (let ((count (mutex-specific *idle-connections-mutex*)))
       (if (< count (max-idle-connections))
           (begin
             (cond ((hash-table-ref/default *idle-connections* uri #f) =>
                    (lambda (oldest-con)
                      (let lp ((head (cdr oldest-con)))
                        (if (eq? (cdr head) oldest-con) ; last?
                            (set-cdr! head (cons con oldest-con))
                            (lp (cdr head))))))
                   (else
                    (let ((new-con (list con)))
                      (set-cdr! new-con new-con) ; (circular-list con)
                      (hash-table-set! *idle-connections* uri new-con))))
             (mutex-specific-set! *idle-connections-mutex* (add1 count)))
           (close-connection! con))))))


(define (close-connection! uri-or-con)
  (cond ((http-connection? uri-or-con)
         (close-input-port (http-connection-inport uri-or-con))
         (close-output-port (http-connection-outport uri-or-con)))
        ((grab-idle-connection! uri-or-con) =>
         (lambda (con)
           (close-connection! con)            ; Close this one
           (close-connection! uri-or-con))))) ; Check for others


(define (close-idle-connections!)
  (with-mutex
   *idle-connections-mutex*
   (lambda ()
     (hash-table-walk
      *idle-connections*
      (lambda (uri conns)
        (let lp ((to-close (cdr conns)))
          (unless (eq? to-close conns)
            (close-input-port (http-connection-inport (car to-close)))
            (close-output-port (http-connection-outport (car to-close)))
            (lp (cdr to-close))))
        (hash-table-delete! *idle-connections* uri)))
     (mutex-specific-set! *idle-connections-mutex* 0))))

;; Imports from the openssl egg, if available
(define (dynamic-import module symbol default)
  (handle-exceptions _ default (eval `(let () (import ,module) ,symbol))))

(define ssl-connect*
  (dynamic-import 'openssl 'ssl-connect* (lambda _ (values #f #f))))

(define (default-server-connector uri proxy)
  (let ((remote-end (or proxy uri)))
    (case (uri-scheme remote-end)
      ((#f http) (tcp-connect (uri-host remote-end) (uri-port remote-end)))
      ((https) (receive (in out)
                   (ssl-connect* hostname: (uri-host remote-end)
                                 port: (uri-port remote-end)
                                 sni-name: #t)
                 (if (and in out)       ; Ugly, but necessary
                     (values in out)
                     (http-client-error
                      'ssl-connect
                      (conc "Unable to connect over HTTPS. To fix this, "
                            "install the openssl egg and try again")
                      (list (uri->string uri))
                      'missing-openssl-egg
                      'request-uri uri 'proxy proxy))))
      (else (http-client-error 'ensure-connection!
                               "Unknown URI scheme"
                               (list (uri-scheme remote-end))
                               'unsupported-uri-scheme
                               'uri-scheme (uri-scheme remote-end)
                               'request-uri uri 'proxy proxy)))))

(define server-connector (make-parameter default-server-connector))

(define (ensure-connection! uri)
  (or (grab-idle-connection! uri)
      (let ((proxy ((determine-proxy) uri)))
        (receive (in out) ((server-connector) uri proxy)
          (make-http-connection uri in out proxy)))))

(define (make-delimited-input-port port len)
  (if (not len)
      port ;; no need to delimit anything
      (let ((pos 0))
        (make-input-port
         (lambda ()                     ; read-char
           (if (= pos len)
               #!eof
               (let ((char (read-char port)))
                 (set! pos (add1 pos))
                 char)))
         (lambda ()                     ; char-ready?
           (or (= pos len) (char-ready? port)))
         (lambda ()                     ; close
           (close-input-port port))
         (lambda ()                     ; peek-char
           (if (= pos len)
               #!eof
               (peek-char port)))
         (lambda (p bytes buf off)      ; read-string!
           (let* ((bytes (min bytes (- len pos)))
                  (bytes-read (read-string! bytes buf port off)))
             (set! pos (+ pos bytes-read))
             bytes-read))
         (lambda (p limit)              ; read-line
           (if (= pos len)
               #!eof
               (let* ((bytes-left (- len pos))
                      (limit (min (or limit bytes-left) bytes-left))
                      (line (read-line port limit)))
                 (unless (eof-object? line)
                         (set! pos (+ pos (string-length line))))
                 line)))))))

(define discard-remaining-data!
  (let ((buf (make-string 1024)))       ; Doesn't matter, discarded anyway
    (lambda (response port)
      ;; If header not available or no response object passed, this reads until EOF
      (let loop ((len (and response
                           (header-value
                            'content-length (response-headers response)))))
        (if len
            (when (> len 0)
              (loop (- len (read-string! len buf port))))
            (when (> (read-string! (string-length buf) buf port) 0)
              (loop #f)))))))

(define (default-prepare-request req)
  (let* ((uri (request-uri req))
         (cookies (get-cookies-for-uri (request-uri req)))
         (h `(,@(if (not (null? cookies)) `((cookie . ,cookies)) '())
              ,@(if (and (client-software) (not (null? (client-software))))
                    `((user-agent ,(client-software)))
                    '()))))
    (update-request req
                    headers: (headers h (request-headers req)))))

(define prepare-request (make-parameter default-prepare-request))

(define (http-client-error loc msg args specific . rest)
  (raise (make-composite-condition
          (make-property-condition 'exn 'location loc 'message msg 'arguments args)
          (make-property-condition 'http)
          (apply make-property-condition specific rest))))

;; RFC 2965, section 3.3.3
(define (cookie-eq? a-name a-info b-name b-info)
  (and (string-ci=? a-name b-name)
       (string-ci=? (alist-ref 'domain a-info) (alist-ref 'domain b-info))
       (equal?      (alist-ref 'path a-info)   (alist-ref 'path b-info))))

(define (store-cookie! cookie-info set-cookie)
  (let loop ((cookie (set-cookie->cookie set-cookie))
             (jar *cookie-jar*))
    (cond
     ((null? jar)
      (set! *cookie-jar* (cons (cons cookie-info cookie) *cookie-jar*))
      *cookie-jar*)
     ((cookie-eq? (car (get-value set-cookie)) cookie-info
                  (car (get-value (cdar jar))) (caar jar))
      (set-car! jar (cons cookie-info cookie))
      *cookie-jar*)
     (else (loop cookie (cdr jar))))))

(define (delete-cookie! cookie-name cookie-info)
  (set! *cookie-jar*
    (remove! (lambda (c)
               (cookie-eq? (car (get-value (cdr c))) (car c)
                           cookie-name cookie-info))
             *cookie-jar*)))

(define (domain-match? uri pattern)
  (let ((target (uri-host uri)))
    (or (string-ci=? target pattern)
        (and (string-prefix? "." pattern)
             (string-suffix-ci? pattern target)))))

(define (path-match? uri path)
  (and (uri-path-absolute? uri)
       (let loop ((path (cdr (uri-path path)))
                  (uri-path (cdr (uri-path uri))))
         (or (null? path)               ; done
             (and (not (null? uri-path))
                  (or (and (string-null? (car path)) (null? (cdr path)))

                      (and (string=? (car path) (car uri-path))
                           (loop (cdr path) (cdr uri-path)))))))))

;; Set-cookie provides some info we don't need to store; strip the
;; nonessential info
(define (set-cookie->cookie info)
  (vector (get-value info)
          (filter (lambda (p)
                    (member (car p) '(domain path version)))
                  (get-params info))))

(define (get-cookies-for-uri uri)
  (let ((uri (if (string? uri) (uri-reference uri) uri)))
    (map cdr
         (sort!
          (filter (lambda (c)
                    (let ((info (car c)))
                     (and (domain-match? uri (alist-ref 'domain info))
                          (member (uri-port uri)
                                  (alist-ref 'port info eq?
                                             (list (uri-port uri))))
                          (path-match? uri (alist-ref 'path info))
                          (if (alist-ref 'secure info)
                              (member (uri-scheme uri) '(https shttp))
                              #t))))
                  *cookie-jar*)
          (lambda (a b)
            (< (length (uri-path (alist-ref 'path (car a))))
               (length (uri-path (alist-ref 'path (car b))))))))))

(define (process-set-cookie! con uri r)
  (let ((prefix-contains-dots?
         (lambda (host pattern)
           (string-index host #\. 0 (string-contains-ci host pattern)))))
    (for-each (lambda (c)
                (and-let* ((path (or (get-param 'path c) uri))
                           ((path-match? uri path))
                           ;; domain must start with dot. Add to intarweb!
                           (dn (get-param 'domain c (uri-host uri)))
                           (idx (string-index dn #\.))
                           ((domain-match? uri dn))
                           ((not (prefix-contains-dots? (uri-host uri) dn))))
                  (store-cookie! `((path . ,path)
                                   (domain . ,dn)
                                   (secure . ,(get-param 'secure c))) c)))
              (header-contents 'set-cookie (response-headers r) '()))
    (for-each (lambda (c)
                (and-let* (((get-param 'version c)) ; required for set-cookie2
                           (path (or (get-param 'path c) uri))
                           ((path-match? uri path))
                           (dn (get-param 'domain c (uri-host uri)))
                           ((or (string-ci=? dn ".local")
                                (and (not (string-null? dn))
                                     (string-index dn #\. 1))))
                           ((domain-match? uri dn))
                           ((not (prefix-contains-dots? (uri-host uri) dn)))
                           ;; This is a little bit too messy for my tastes...
                           ;; Can't use #f because that would shortcut and-let*
                           (ports-value (get-param 'port c 'any))
                           (ports (if (eq? ports-value #t)
                                      (list (uri-port uri))
                                      ports-value))
                           ((or (eq? ports 'any)
                                (member (uri-port uri) ports))))
                  (store-cookie! `((path . ,path)
                                   (domain . ,dn)
                                   (port . ,(if (eq? ports 'any) #f ports))
                                   (secure . ,(get-param 'secure c))) c)))
              (header-contents 'set-cookie2 (response-headers r) '()))))

(define (get-username/password for-request-header for-uri for-realm)
  (if (eq? for-request-header 'authorization)
      ((determine-username/password) for-uri for-realm)
      ((determine-proxy-username/password) for-uri for-realm)))

;;; TODO: We really, really should get rid of "writer" here.  Some kind of
;;; generalized way to get the digest is required.  Jeez, HTTP sucks :(
(define (basic-authenticator response response-header
                             new-request request-header uri realm writer)
  (receive (username password)
    (get-username/password request-header uri realm)
    (and username
         (update-request
          new-request
          headers: (headers `((,request-header
                               #(basic ((username . ,username)
                                        (password . ,(or password ""))))))
                            (request-headers new-request))))))

(define (digest-authenticator response response-header
                              new-request request-header uri realm writer)
  (receive (username password)
    (get-username/password request-header uri realm)
    (and username
         (let* ((hashconc
                 (lambda args
                   (string->md5sum (string-join (map ->string args) ":"))))
                (authless-uri (update-uri (request-uri new-request)
                                          username: #f password: #f))
                ;; TODO: domain handling
                (h (response-headers response))
                (nonce (header-param 'nonce response-header h))
                (opaque (header-param 'opaque response-header h))
                (stale (header-param 'stale response-header h))
                ;; TODO: "md5-sess" algorithm handling
                (algorithm (header-param 'algorithm response-header h))
                (qops (header-param 'qop response-header h '()))
                (qop (cond ; Pick the strongest of the offered options
                      ((member 'auth-int qops) 'auth-int)
                      ((member 'auth qops) 'auth)
                      (else #f)))
                (cnonce (and qop (hashconc (current-seconds) realm)))
                (nc (and qop 1)) ;; TODO
                (ha1 (hashconc username realm (or password "")))
                (ha2 (if (eq? qop 'auth-int)
                         (hashconc (request-method new-request)
                                   (uri->string authless-uri)
                                   ;; Generate digest from writer's output
                                   ;; TODO: This should not generate one
                                   ;; large string but use a custom port.
                                   ;; Ideally we extract this from
                                   ;; this egg into another one.
                                   (string->md5sum
                                    (call-with-output-string
                                     (lambda (p)
                                       (writer
                                        (update-request new-request port: p))))))
                         (hashconc (request-method new-request)
                                   (uri->string authless-uri))))
                (digest
                 (case qop
                   ((auth-int auth)
                    (let ((hex-nc (string-pad (number->string nc 16) 8 #\0)))
                      (hashconc ha1 nonce hex-nc cnonce qop ha2)))
                   (else
                    (hashconc ha1 nonce ha2)))))
           (update-request new-request
                           headers: (headers
                                     `((,request-header
                                        #(digest ((username . ,username)
                                                  (uri . ,authless-uri)
                                                  (realm . ,realm)
                                                  (nonce . ,nonce)
                                                  (cnonce . ,cnonce)
                                                  (qop . ,qop)
                                                  (nc . ,nc)
                                                  (response . ,digest)
                                                  (opaque . ,opaque)))))
                                     (request-headers new-request)))))))

(define http-authenticators
  (make-parameter `((basic . ,basic-authenticator)
                    (digest . ,digest-authenticator))))

(define (authenticate-request request response writer proxy-uri)
  (and-let* ((type (if (= (response-code response) 401) 'auth 'proxy))
             (resp-header (if (eq? type 'auth)
                              'www-authenticate
                              'proxy-authenticate))
             (req-header (if (eq? type 'auth)
                             'authorization
                             'proxy-authorization))
             (authtype (header-value resp-header (response-headers response)))
             (realm (header-param 'realm resp-header (response-headers response)))
             (auth-uri (if (eq? type 'auth) (request-uri request) proxy-uri))
             (authenticator (or (alist-ref authtype (http-authenticators))
                                ;; Should we really raise an error?
                                (http-client-error 'authenticate-request
                                                   "Unknown authentication type"
                                                   (list authtype)
                                                   'unknown-authtype
                                                   'authtype authtype
                                                   'request request))))
    (authenticator response resp-header request req-header
                   auth-uri realm writer)))

(define (call-with-response req writer reader)
  (let loop ((attempts 0)
             (redirects 0)
             (req req))
    (let* ((uri (request-uri req))
           (con (ensure-connection! uri)))
      (condition-case
          (let* ((req ((prepare-request)
                       (update-request
                        req
                        headers: (headers
                                  `((host ,(cons (uri-host uri)
                                                 (and (not (uri-default-port? uri))
                                                      (uri-port uri)))))
                                  (request-headers req))
                        port: (http-connection-outport con))))
                 ;; No outgoing URIs should ever contain credentials or fragments
                 (req-uri (update-uri uri fragment: #f username: #f password: #f))
                 ;; RFC1945, 5.1.2: "The absoluteURI form is only allowed
                 ;; when the request is being made to a proxy."
                 ;; RFC2616 is a little more regular (hosts MUST accept
                 ;; absoluteURI), but it says "HTTP/1.1 clients will only
                 ;; generate them in requests to proxies." (also 5.1.2)
                 (req-uri (if (http-connection-proxy con)
                              req-uri
                              (update-uri req-uri
                                          host: #f port: #f scheme: #f)))
                 ;; Update path only when it needs to be updated, to
                 ;; avoid unnecessarily mangling it (see #1448)
                 (req-uri (if (or (http-connection-proxy con)
                                  (not (memq (uri-path req-uri) '(() #f))))
                              req-uri
                              (update-uri req-uri path: '(/ ""))))
                 (request (write-request (update-request req uri: req-uri)))
                 ;; Writer should be prepared to be called several times
                 ;; Maybe try and figure out a good way to use the
                 ;; "Expect: 100-continue" header to prevent too much writing?
                 ;; Unfortunately RFC2616 says it's unreliable (8.2.3)...
                 ;;
                 ;; TODO: Should we avoid calling "writer" if
                 ;; request-has-message-body? returns false?
                 (_ (begin (writer request)
                           (flush-output (request-port req))
                           ;; Signal end of file when we can.
                           (unless (keep-alive? request)
                             (close-output-port
                              (http-connection-outport con)))))
                 (response (read-response (http-connection-inport con)))
                 (cleanup!
                  (lambda (clear-response-data?)
                    (when clear-response-data?
                      (discard-remaining-data! response
                                               (response-port response)))
                    (if (and (keep-alive? request)
                             (keep-alive? response))
                        (maybe-add-idle-connection! uri con)
                        (close-connection! con)))))
            (when response (process-set-cookie! con uri response))
            (case (and response (response-code response))
              ((#f)
               ;; If the connection is closed prematurely, we SHOULD
               ;; retry, according to RFC2616, section 8.2.4.  Currently
               ;; don't do "binary exponential backoff", which we MAY do.
               (if (and (or (not (max-retry-attempts)) ; unlimited?
                            (< attempts (max-retry-attempts)))
                        ((retry-request?) req))
                   (loop (add1 attempts) redirects req)
                   (http-client-error 'send-request
                                      "Server closed connection before sending response"
                                      (list (uri->string uri))
                                      'premature-disconnection
                                      'uri uri 'request req)))
              ;; TODO: According to spec, we should provide the user
              ;; with a choice when it's not a GET or HEAD request...
              ((301 302 303 307)
               (cleanup! #t)
               ;; Maybe we should switch to GET on 302 too?  It's not compliant,
               ;; but very widespread and there's enough software that depends
               ;; on that behaviour, which might break horribly otherwise...
               (when (= (response-code response) 303)
                 (request-method-set! req 'GET) ; Switch to GET
                 ;; Is this OK, or avoid calling writer (see above)?
                 (let ((h (headers '((content-length 0))
                                   (request-headers req))))
                   (request-headers-set! req h))
                 (set! writer (lambda x void)))
               (let* ((loc-uri (header-value 'location
                                             (response-headers response)))
                      (new-uri (uri-relative-to loc-uri uri)))
                 (if (or (not (max-redirect-depth)) ; unlimited?
                         (< redirects (max-redirect-depth)))
                     (loop attempts
                           (add1 redirects)
                           (update-request req uri: new-uri))
                     (http-client-error 'send-request
                                        "Maximum number of redirects exceeded"
                                        (list (uri->string uri))
                                        'redirect-depth-exceeded
                                        'uri uri 'new-uri new-uri
                                        'request req 'response response))))
              ;; TODO: Test this
              ((305)             ; Use proxy (for this request only)
               (cleanup! #t)
               (let ((old-determine-proxy (determine-proxy))
                     (proxy-uri (header-value 'location (response-headers response))))
                 (parameterize ((determine-proxy
                                 (lambda _
                                   ;; Reset determine-proxy so the proxy is really
                                   ;; used for only this one request.
                                   ;; Yes, this is a bit of a hack :)
                                   (determine-proxy old-determine-proxy)
                                   proxy-uri)))
                   (loop attempts redirects req))))
              ((401 407) ; Unauthorized, Proxy Authentication Required
               (cond ((and (or (not (max-retry-attempts)) ; unlimited?
                               (< attempts (max-retry-attempts)))
                           (authenticate-request req response writer
                                                 (http-connection-proxy con)))
                      => (lambda (new-req)
                           (cleanup! #t)
                           (loop (add1 attempts) redirects new-req)))
                     (else ;; pass it on, we can't throw an error here
                      (let ((data (reader response)))
                        (values data uri response)))))
              (else (let ((data (reader response)))
                      (cleanup! #f)
                      (values data uri response)))))
        (exn (exn i/o net)
             ;; Try to recover from bad connections if we may retry.
             (close-connection! con)
             (if (and (or (not (max-retry-attempts)) ; unlimited?
                          (< attempts (max-retry-attempts)))
                      ((retry-request?) req))
                 (loop (add1 attempts) redirects req)
                 (raise exn)))
        (exn ()
           ;; Never leave the port in an unknown/inconsistent state
           ;; (the error could have occurred while reading, so there
           ;;  might be data left in the buffer)
           (close-connection! con)
           (raise exn))))))

(define (kv-ref l k #!optional default)
  (let ((rest (and (pair? l) (memq k l))))
    (if (and rest (pair? (cdr rest))) (cadr rest) default)))

;; This really, really sucks
;; TODO: This crap probably belongs in its own egg?  Perhaps later when
;; we have server-side handling for this too.
(define (prepare-multipart-chunks boundary entries)
  (append
   (map (lambda (entry)
          (if (not (cdr entry))         ; discard #f values
              '()
              (let* ((keys (cdr entry))
                     (file (kv-ref keys file:))
                     (filename (or (kv-ref keys filename:)
                                   (and (port? file) (port-name file))
                                   (and (string? file) file)))
                     (filename (and filename
                                    (pathname-strip-directory filename)))
                     (h (headers `((content-disposition
                                    #(form-data ((name . ,(car entry))
                                                 (filename . ,filename))))
                                   ,@(if filename
                                         '((content-type application/octet-stream))
                                         '()))))
                     (hs (call-with-output-string
                           (lambda (s)
                             (unparse-headers
                              ;; Allow user headers to override ours
                              (headers (kv-ref keys headers: '()) h) s)))))
                (list "--" boundary "\r\n" hs "\r\n"
                      (cond ((string? file) (cons 'file file))
                            ((port? file) (cons 'port file))
                            ((eq? keys #t) "")
                            (else (->string keys)))
                  ;; The next boundary must always start on a new line
                  "\r\n"))))
        entries)
   (list (list "--" boundary "--\r\n"))))

(define (write-chunks output-port entries)
  (for-each (lambda (entry)
              (for-each (lambda (chunk)
                          (if (pair? chunk)
                              (let ((p (if (eq? 'file (car chunk))
                                           (open-input-file (cdr chunk))
                                           ;; Should be a port otherwise
                                           (cdr chunk))))
                                (handle-exceptions exn
                                  (begin (close-input-port p) (raise exn))
                                  (sendfile p output-port))
                                (close-input-port p))
                              (display chunk output-port)))
                        entry))
            entries))

(define (calculate-chunk-size entries)
  (call/cc
   (lambda (return)
     (fold (lambda (chunks total-size)
             (fold (lambda (chunk total-size)
                     (if (pair? chunk)
                         (if (eq? 'port (car chunk))
                             ;; Should be a file otherwise.
                             ;; We can't calculate port lengths.
                             ;; Let's just punt and hope the server
                             ;; won't return "411 Length Required"...
                             ;; (TODO: maybe try seeking it?)
                             (return #f)
                             (+ total-size (file-size (cdr chunk))))
                         (+ total-size (string-length chunk))))
                   total-size
                   chunks))
           0 entries))))

(define (call-with-input-request* uri-or-request writer reader)
  (let* ((type-headers '())
         (uri (cond ((uri-reference? uri-or-request) uri-or-request)
                    ((string? uri-or-request) (uri-reference uri-or-request))
                    ((request? uri-or-request) (request-uri uri-or-request))
                    (else #f)))
         (_ (unless (uri? uri)
              (http-client-error
               'call-with-input-request
               (if (uri-reference? uri)
                   "Bad argument: URI must be a proper URI, not a relative reference (protocol and host must be set)"
                   "The first argument must be either an uri-common object, an intarweb request object, or an URI string")
               (list uri-or-request writer reader)
               'bad-uri 'uri uri-or-request)))
	 (req (if (request? uri-or-request)
                  uri-or-request
                  (make-request uri: uri method: (if writer 'POST 'GET))))
         (chunks (cond
                  ((string? writer) (list (list writer)))
                  ((and (list? writer)
                        (any (lambda (x)
                               (and (pair? x) (pair? (cdr x))
                                    (eq? (cadr x) file:)))
                             writer))
                   (let ((bd (conc "----------------Multipart-=_"
                                   (gensym 'boundary) "=_=" (current-process-id)
                                   "=-=" (current-seconds))))
                     (set! type-headers `((content-type #(multipart/form-data ((boundary . ,bd))))))
                     (prepare-multipart-chunks bd writer)))
                  ;; Default to "&" because some servers choke on ";"
                  ((list? writer)
                   (set! type-headers
                     '((content-type application/x-www-form-urlencoded)))
                   (list (list (or (form-urlencode writer separator: "&")
                                   (http-client-error
                                    'call-with-input-request
                                    "Invalid form data!"
                                    (list (uri->string uri) writer reader)
                                    'form-data-error
                                    'request req
                                    'form-data writer)))))
                  (else #f)))
         ;; If the size is known, the user can supply a content-length
         ;; header to avoid chunking.  For HTTP/1.0 we never chunk.
         (need-chunked-encoding?
          (and (= 1 (request-major req)) (= 1 (request-minor req))
               (not (header-value 'content-length (request-headers req)))))
         (size-headers
          (cond
           (chunks
            (let ((size (calculate-chunk-size chunks)))
              (cond
               (size `((content-length ,size)))
               (need-chunked-encoding? `((transfer-encoding chunked)))
               (else '()))))
           ;; We can't calculate the size except by
           ;; calling the procedure, but that's wasteful.
           ((and need-chunked-encoding? (procedure? writer))
            `((transfer-encoding chunked)))
           (else '())))
         (req (update-request
               req headers: (headers `(,@size-headers ,@type-headers)
                                     (request-headers req)))))
    (call-with-response
     req
     (cond (chunks (lambda (r)
                     (write-chunks (request-port r) chunks)
                     (finish-request-body r)))
           ((procedure? writer)
            (lambda (r)
              (writer (request-port r))
              (finish-request-body r)))
           (else (lambda x (void))))
     (lambda (response)
       (let ((port (make-delimited-input-port
                    (response-port response)
                    (header-value 'content-length (response-headers response))))
             (body? ((response-has-message-body-for-request?) response req)))
         (if (= 200 (response-class response)) ; Everything cool?
             (let ((result (and body? reader (reader port response))))
               (when body? (discard-remaining-data! #f port))
               result)
             (http-client-error
              'call-with-input-request
              ;; Message
              (sprintf (case (response-class response)
                         ((400) "Client error: ~A ~A")
                         ((500) "Server error: ~A ~A")
                         (else "Unexpected server response: ~A ~A"))
                (response-code response) (response-reason response))
	      ;; arguments
	      (list (request-method req) (uri->string uri))
              ;; Specific type
              (case (response-class response)
                ((400) 'client-error)
                ((500) 'server-error)
                (else 'unexpected-server-response))
              'response response
              'body (and body? (read-string #f port)))))))))

(define (call-with-input-request uri-or-request writer reader)
  (call-with-input-request* uri-or-request writer (lambda (p r) (reader p))))

(define (with-input-from-request uri-or-request writer reader)
  (call-with-input-request uri-or-request
                           (if (procedure? writer)
                               (lambda (p) (with-output-to-port p writer))
                               writer) ;; Assume it's an alist or #f
                           (lambda (p) (with-input-from-port p reader))))

)
