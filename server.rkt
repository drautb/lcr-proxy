#lang racket

(require (planet dmac/spin)
         net/uri-codec
         net/url
         web-server/servlet)

(define PORT
  (let ([port (environment-variables-ref (current-environment-variables) #"PORT")])
    (if port
        (string->number (bytes->string/utf-8 port))
        5000)))

(define ORIGIN #"Origin")
(define HTTPS #"https")
(define SET-COOKIE #"Set-Cookie")
(define CORS-ALLOW-ORIGIN #"Access-Control-Allow-Origin")
(define CORS-ALLOW-CREDENTIALS #"Access-Control-Allow-Credentials")
(define TRUE #"true")
(define EXCLUDED-REQUEST-HEADERS (list #"Host"))
(define EXCLUDED-RESPONSE-HEADERS (list #"Transfer-Encoding"
                                        #"Content-Length"
                                        SET-COOKIE))

(define (proxy-response-maker status headers body)
  (response status
            (status->message status)
            (current-seconds)
            #f
            headers
            (λ (out-port)
              (when body
                (write-bytes (force body) out-port)))))

(define (proxy-get path handler)
  (define-handler "GET" path handler proxy-response-maker))

(define (proxy-post path handler)
  (define-handler "POST" path handler proxy-response-maker))

(define (status) (λ ()
                   (log-info "Status requested.")
                   "alive"))

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
                              EXCLUDED-REQUEST-HEADERS)))
               (request-headers/raw req))))

(define (extract-origin req)
  (define (find-origin headers)
    (cond [(empty? headers) #f]
          [(equal? (header-field (first headers)) ORIGIN) (header-value (first headers))]
          [else (find-origin (rest headers))]))
  (find-origin (request-headers/raw req)))

(define (forward-response-headers headers origin)
  (define cookie-attributes (if (regexp-match #"https" origin)
                                ";secure; path=/;"
                                "; path=/;"))
  (define (find-set-cookie headers)
    (cond [(empty? headers) #f]
          [(regexp-match SET-COOKIE (first headers)) (first headers)]
          [else (find-set-cookie (rest headers))]))
  (let ([set-cookie-header (build-header-from-str 
                             (string->bytes/utf-8 
                               (string-replace (bytes->string/utf-8 (find-set-cookie headers)) 
                                               ";secure; path=/; domain=.lds.org" 
                                               cookie-attributes)))]
        [forwarded-headers 
         (filter (λ (h) 
                   (not (member (header-field h)
                                EXCLUDED-RESPONSE-HEADERS)))
                 (map build-header-from-str headers))])
    (define final-headers 
      (if origin
          (append forwarded-headers 
                  (list (header CORS-ALLOW-CREDENTIALS TRUE) 
                        (header CORS-ALLOW-ORIGIN origin)
                        set-cookie-header))
          forwarded-headers))
    final-headers))

(define (proxy-request orig-req method url [data #f]) 
  (log-info "Making upstream request method=~a url=~a" method url)
  (let-values ([(status headers in-port)
                (http-sendrecv/url (string->url url)
                                   #:method method
                                   #:headers (forward-request-headers orig-req)
                                   #:data data)])
    (log-info "Upstream response for method=~a url=~a response=~a" method url (extract-status-code status))
    (list (extract-status-code status)
          (forward-response-headers headers (extract-origin orig-req))
          (port->bytes in-port))))

;; Healthcheck
(get "/" (status))
(get "/status" (status))

;; Login endpoint
(proxy-post "/login.html"
            (λ (req)
              (let* ([username (params req 'username)]
                     [password (params req 'password)]
                     [data (alist->form-urlencoded
                             (list (cons 'username username)
                                   (cons 'password password)))])
                (proxy-request req #"POST" "https://signin.lds.org/login.html" data))))

;; Current user information
(proxy-get "/htvt/services/v1/user/currentUser"
           (λ (req)
             (proxy-request req #"GET" "https://www.lds.org/htvt/services/v1/user/currentUser")))

;; Member information
(proxy-get "/htvt/services/v1/:unit-number/members"
           (λ (req)
             (proxy-request req #"GET"
                            (string-append "https://www.lds.org/htvt/services/v1/"
                                           (params req 'unit-number) "/members"))))

(log-info "Starting server on port ~a" PORT)
(run #:port PORT
     #:listen-ip #f)
