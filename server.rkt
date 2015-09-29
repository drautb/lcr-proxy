#lang racket

(require (planet dmac/spin)
         net/http-client
         net/uri-codec
         web-server/servlet
         json)

(define LOGIN-URL "https://signin.lds.org/login.html")
(define MEMBER-INFO-URL "https://www.lds.org/htvt/services/v1/' + PP6_UNIT_ID + '/members")

(define PORT
  (let ([port (environment-variables-ref (current-environment-variables) #"PORT")])
    (if port
        (string->number (bytes->string/utf-8 port))
        5000)))

(define (json-response-maker status headers body)
  (response (if (eq? body #f) 404 status)
            (status->message status)
            (current-seconds)
            #"application/json; charset=utf-8"
            headers
            (lambda (op)
              (when body
                (write-json (force body) op)))))

(define (proxy-response-maker status headers body)
  (response status
            (status->message status)
            (current-seconds)
            #f
            headers
            (λ (out-port)
              (when body
                (write-bytes (force body) out-port)))))

(define (json-get path handler)
  (define-handler "GET" path handler json-response-maker))

(define (proxy-get path handler)
  (define-handler "GET" path handler proxy-response-maker))

(define (proxy-post path handler)
  (define-handler "POST" path handler proxy-response-maker))

(define (status) (λ ()
                   (log-info "Status requested.")
                   (make-hash (list (cons 'status "alive")))))

(define (extract-status-code status)
  (string->number (first (regexp-match #px"\\d{3}" (bytes->string/utf-8 status)))))

(define (build-header-from-str header-bytes)
  (let ([pieces (string-split (bytes->string/utf-8 header-bytes) ":")])
    (header (string->bytes/utf-8 (first pieces))
            (string->bytes/utf-8 (string-join (rest pieces) ":")))))

(define (build-str-from-header h)
  (string-append (bytes->string/utf-8 (header-field h)) ": " 
                 (bytes->string/utf-8 (header-value h))))

(define (forward-request-headers req)
  (map build-str-from-header 
       (filter (λ (h) 
                 (not (member (header-field h)
                              (list #"Host"))))
               (request-headers/raw req))))

(define (forward-response-headers headers)
  (filter (λ (h) 
            (not (member (header-field h)
                         (list #"Transfer-Encoding"))))
          (map build-header-from-str headers)))

;; Healthcheck
(json-get "/" (status))
(json-get "/status" (status))

;; Login endpoint
(proxy-post "/login.html"
            (λ (req)
              (let ([username (params req 'username)]
                    [password (params req 'password)])
                (log-info "Attempting to login. user=~a" username)
                (let-values ([(status headers in-port) 
                              (http-sendrecv "signin.lds.org"
                                             "/login.html"
                                             #:ssl? #t
                                             #:headers (forward-request-headers req)
                                             #:method #"POST"
                                             #:data (alist->form-urlencoded
                                                      (list (cons 'username username)
                                                            (cons 'password password))))])
                  (log-info "Login result=~a" status)
                  (list (extract-status-code status)
                        (forward-response-headers headers)
                        (port->bytes in-port))))))

;; Current user information
(proxy-get "/htvt/services/v1/user/currentUser"
           (λ (req)
             (log-info "GET /htvt/services/v1/user/currentUser")
             (let-values ([(status headers in-port) 
                           (http-sendrecv "www.lds.org"
                                          "/htvt/services/v1/user/currentUser"
                                          #:ssl? #t
                                          #:headers (forward-request-headers req))])
               (log-info "GET /htvt/services/v1/user/currentUser result=~a" status)
               (list (extract-status-code status)
                     (forward-response-headers headers)
                     (port->bytes in-port)))))

;; Member information
(proxy-get "/htvt/services/v1/:unit-number/members"
           (λ (req)
             (let* ([unit-number (params req 'unit-number)]
                    [path (string-append "/htvt/services/v1/" unit-number "/members")])
               (log-info "GET ~a" path)
               (let-values ([(status headers in-port) 
                             (http-sendrecv "www.lds.org"
                                            path
                                            #:ssl? #t
                                            #:headers (forward-request-headers req))])
                 (log-info "GET ~a result=~a" path status)
                 (list (extract-status-code status)
                       (forward-response-headers headers)
                       (port->bytes in-port))))))

(log-info "Starting server on port ~a" PORT)
(run #:port PORT
     #:listen-ip #f)
