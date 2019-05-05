;; http-client test library.  This adds some helpers for setting up
;; fake connections and logging the requests and responses.

;; TODO: Test HTTPS somehow?

(import test uri-common intarweb srfi-1 srfi-18 (chicken tcp)
        (chicken string) (chicken io) (chicken file) (chicken format))

;; From intarweb
(define-syntax test-error*
  (syntax-rules ()
    ((_ ?msg (?error-type ...) ?expr)
     (let-syntax ((expression
                   (syntax-rules ()
                     ((_ ?expr)
                      (condition-case (begin ?expr "<no error thrown>")
                                      ((?error-type ...) '(?error-type ...))
                                      (exn () (##sys#slot exn 1)))))))
       (test ?msg '(?error-type ...) (expression ?expr))))
    ((_ ?msg ?error-type ?expr)
     (test-error* ?msg (?error-type) ?expr))
    ((_ ?error-type ?expr)
     (test-error* (sprintf "~S" '?expr) ?error-type ?expr))))

(define-record log request body)

(define server-port #f)

(server-connector (lambda (uri proxy)
                    (tcp-connect "localhost" server-port)) )

;; These need to be reasonably high to avoid lots of errors on slow
;; VMs and some OSes (FreeBSD in particular?), see also Salmonella.
;; At least 100 seems to be too low, so we aim high and set it to 500.
(tcp-read-timeout 500)
(tcp-write-timeout 500)

;; Set up a number of fake connections to a "server", with predefined
;; responses for each (expected) request.
(define (with-server-responses thunk . responses)
  (let* ((response-count (length responses))
         (logs '())
         (listener (tcp-listen 0 0 "localhost"))
         (server-thread
          (thread-start!
           (make-thread
            (lambda ()
              (let lp ()
                (if (null? responses)
                    (tcp-close listener)
                    (receive (in out) (tcp-accept listener)
                      (let* ((req (read-request in))
                             (h (request-headers req))
                             (log (make-log req #f))
                             (response (car responses)))

                        (when ((request-has-message-body?) req)
                          (let* ((len (header-value 'content-length h))
                                 (body (read-string len (request-port req))))
                            (log-body-set! log body)))
                        (set! logs (cons log logs))
                        (set! responses (cdr responses))
                        (display response out)
                        (close-output-port out)
                        (lp))))))
            'server-thread))))

    (set! server-port (tcp-listener-port listener))

    ;; TODO: Figure out how to ensure connections get closed correctly
    (dynamic-wind
        void
        thunk
        (lambda ()
          (handle-exceptions exn (thread-terminate! server-thread)
            ;; To close idle connections here to catch a regression
            ;; where we would loop endlessly...
            (close-idle-connections!)
            (thread-join! server-thread 0)) ))
    
    ;; Return the accumulated logs if all went well
    (if (not (= (length logs) response-count))
        (error (sprintf "Not enough requests.  Expected ~A responses, but logged ~A requests!" response-count (length logs)))
        (reverse logs)) ))

(define (with-server-response thunk response)
  (car (with-server-responses thunk response)))
