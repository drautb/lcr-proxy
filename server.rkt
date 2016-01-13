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
(define HOST #"Host")
(define LOCATION #"Location")
(define CORS-ALLOW-ORIGIN #"Access-Control-Allow-Origin")
(define CORS-ALLOW-CREDENTIALS #"Access-Control-Allow-Credentials")
(define TRUE #"true")
(define EXCLUDED-REQUEST-HEADERS (list HOST))
(define EXCLUDED-RESPONSE-HEADERS (list #"Transfer-Encoding"
                                        #"Content-Length"
                                        LOCATION
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

(define (build-header-from-str header-bytes)
  (let ([pieces (string-split (bytes->string/utf-8 header-bytes) ":")])
    (header (string->bytes/utf-8 (first pieces))
            (string->bytes/utf-8 (string-join (rest pieces) ":")))))

(define (build-str-from-header h)
  (string-append (bytes->string/utf-8 (header-field h)) ": "
                 (bytes->string/utf-8 (header-value h))))


(define (extract-status-code status)
  (string->number (first (regexp-match #px"\\d{3}" (bytes->string/utf-8 status)))))

(define (find-header header-name headers)
  (cond [(empty? headers) #f]
        [(equal? header-name (header-field (first headers))) (first headers)]
        [else (find-header header-name (rest headers))]))

(define (find-header-str header-name headers)
  (find-header header-name (map build-header-from-str headers)))

(define (extract-origin req)
  (let ([origin (find-header ORIGIN (request-headers/raw req))])
    (if origin
        (header-value origin)
        #f)))

(define (extract-host req)
  (let ([host (find-header HOST (request-headers/raw req))])
    (if host
        (header-value host)
        #f)))

(define (generate-cors-headers origin)
  (if origin (list (header CORS-ALLOW-CREDENTIALS TRUE)
                   (header CORS-ALLOW-ORIGIN origin))
      '()))

(define (munge-set-cookie-header headers origin)
  (define should-be-secure (and origin (regexp-match HTTPS origin)))
  (define (find-set-cookie headers)
    (cond [(empty? headers) #f]
          [(regexp-match SET-COOKIE (first headers)) (first headers)]
          [else (find-set-cookie (rest headers))]))
  (let ([cookie-attributes (if should-be-secure ";secure; path=/;" "; path=/;")]
        [set-cookie-header (find-set-cookie headers)])
    (if set-cookie-header
        (list (build-header-from-str
                (regexp-replace #rx";secure; path=/; domain=.lds.org"
                                set-cookie-header
                                cookie-attributes)))
        (begin (log-error "Failed to find Set-Cookie header. Headers: ~a" headers)
               '()))))

(define (munge-location-header headers host)
  (define (https->http location-header)
    (if (or (regexp-match #rx"localhost" (header-value location-header))
            (regexp-match #rx"0\\.0\\.0\\.0" (header-value location-header)))
        (header LOCATION (regexp-replace #rx"https://" (header-value location-header) #"http://"))
        location-header))
  (let ([location-header (find-header-str LOCATION headers)])
    (if location-header
        (list
          (https->http
            (build-header-from-str
              (bytes-append LOCATION #":"
                            (regexp-replace #px"//(\\w|\\.)*/"
                                            (header-value location-header)
                                            (bytes-append #"//" host #"/"))))))
        (begin (log-warning "Failed to find Location header. (Fine if this was a 200 response) Headers: ~a" headers)
               '()))))

(define (forward-request-headers req)
  (map build-str-from-header
       (filter (λ (h)
                 (not (member (header-field h)
                              EXCLUDED-REQUEST-HEADERS)))
               (request-headers/raw req))))

(define (forward-response-headers headers origin host)
  (append (munge-set-cookie-header headers origin)
          (munge-location-header headers host)
          (generate-cors-headers origin)
          (filter (λ (h)
                    (not (member (header-field h)
                                 EXCLUDED-RESPONSE-HEADERS)))
                  (map build-header-from-str headers))))

(define (proxy-request orig-req method url [data #f])
  (log-info "Making upstream request method=~a url=~a" method url)
  (let-values ([(status headers in-port)
                (http-sendrecv/url (string->url url)
                                   #:method method
                                   #:headers (forward-request-headers orig-req)
                                   #:data data)])
    (log-info "Upstream response for method=~a url=~a response=~a" method url (extract-status-code status))
    (list (extract-status-code status)
          (forward-response-headers headers
                                    (extract-origin orig-req)
                                    (extract-host orig-req))
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

;; Logout endpoint
(proxy-get "/SSOSignIn/logout.jsp"
           (λ (req)
             (proxy-request req #"GET" "https://signin.lds.org/SSOSignIn/logout.jsp")))

;; Redirected to Login Page
(proxy-get "/sso/UI/Login"
           (λ (req)
             (proxy-request req #"GET" "https://ident.lds.org/sso/UI/Login")))

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
