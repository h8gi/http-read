(use tcp6 openssl uri-common defstruct base64 message-digest md5 sha2)
(include "parser.scm")
(define http-read-debug (make-parameter #f))
(define user-agent-name "http-read v1.0")
(set! char-set:uri-unreserved
      (string->char-set "-.0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~"))

;;; uri = (uri-reference str)
(define (connect-to-server uri)
  (let ([host (uri-host uri)]
        [scheme (uri-scheme uri)]
        [port (uri-port uri)])
    (if (http-read-debug) (printf "connect to: ~A~%" (uri->string uri)))
    (receive (i o)
        (case scheme
          [(http) (tcp-connect host port)]
          [(https) (ssl-connect host port)])
      (if (and i o) (values i o)
          (error 'connect-to-server uri)))))

(define (process-server uri path header body method)
  (receive (in out) (connect-to-server uri)
    (dynamic-wind
	(lambda ()
	  (send-request path
			header
			body
			#:method method
			#:port out))
	(lambda ()
	  (get-response in (eq? method 'head)))
	(lambda ()
	  (close-input-port in)
	  (close-output-port out)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; all
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; authorization
(define (http-read uri-or-str
		   #!key
		   (header '())
		   (method 'get)
		   (query '())
		   (proxy #f)
		   (user #f)
		   (password #f))
  (let* ([response (http-read-without-auth uri-or-str
					   #:header header
					   #:method method
					   #:query query
					   #:proxy proxy)]
	 [status (car (response-status response))])
    (cond [(and (= 401 status) user password) ;if auth failed
	   (let* ([www-authenticate-line (header-ref
					  (response-header response)
					  'www-authenticate)]
		  [auth-alist (parse-www-authenticate-header www-authenticate-line)])
	     (pp auth-alist)
	     (http-read-without-auth
	      uri-or-str
	      #:method method
	      #:query query
	      #:proxy proxy
	      #:header 
	      (header-update header 'authorization
			     (cond
			      [(alist-ref 'basic auth-alist)
			       =>
			       (cut basic user password <>)]
			      [(alist-ref 'digest auth-alist)
			       =>
			       (cut digest user password <>)]
			      [else (error "Unkonow authorization scheme" auth-alist)]))))]
	  [else response])))

(define (parse-www-authenticate-header line)
  (parse-string line p.www-authenticate))

(define (basic user-id password params-alst)
  (string-append "Basic " (base64-encode (string-append user-id ":" password))))

(define (digest user-id password params-alst)
  (let*-values ([(nonce)  (alist-ref "nonce" params-alst string=?)]
		[(opaque) (alist-ref "opaque" params-alst string=?)]
		[(algorithm sess?) (parse-algorithm (alist-ref "algorithm" params-alst string=?))]
		[(qop) (alist-ref "qop" params-alst string=?)]
		[(auth-int?) (irregex-search "auth-int" qop)])
    (pp (list nonce opaque algorithm sess? qop auth-int?))
    (string-append "Digest " "FOO")))

(define (parse-algorithm algorithm)
  (let ([m (and algorithm (irregex-match "(.+)-sess" algorithm))])
    (if m
	(values (irregex-match-substring m 1) #t)
	(values algorithm #f))))

(define (http-read-without-auth uri-or-str 
				#!key
				(header '())
				(method 'get)
				(query '())
				(proxy #f))
  (let* ([uri (trim-uri-or-str uri-or-str)]
	 [proxy (if proxy (trim-uri-or-str proxy) #f)]
	 [header (header-update
		  (add-ua-to-header
		   ;; if proxy, header doesn't contain host header
		   (if proxy header
		       (add-host-to-header header uri)))
		  'connection "close")])
    (case method
      [(get head delete)
       ;; GET, HEAD, DELETE
       (let ([path (make-request-path uri
				      (append (uri-query uri) query)
				      proxy)])
	 (process-server (or proxy uri) path header "" method))]
      [(put post)
       ;; POST, PUT
       (let* ([path (make-request-path uri
				       (uri-query uri)
				       proxy)]
	      [body (or (form-urlencode query #:separator (char-set #\&)) "")]
	      [content-length (string-length body)]
	      [header (header-update header 'content-length content-length)])
	 (process-server (or proxy uri) path header body method))]
      [else (error "method must be one of (get head delete put post)" method)])))

;;; if proxy absolute, else relative path
(define (make-request-path uri query proxy)
  (let ([path ((if proxy uri->string uri-path-string) uri)])
    (if (null? query) path
	(string-append path "?"
		       (form-urlencode query #:separator (char-set #\&))))))

(define (add-host-to-header header abs-uri)
  (cond [(header-ref header 'host) header]
	[else (header-update header 'host (uri-host abs-uri))]))

(define (add-ua-to-header header)
  (if (header-ref header 'user-agent) header
      (header-update header 'user-agent user-agent-name)))

(define (trim-uri-or-str uri-or-str)
  (let ([uri (if (uri? uri-or-str) uri-or-str
		 (uri-reference uri-or-str))])
    (cond [(absolute-uri? uri) uri]
	  [(string? uri-or-str) (uri-reference (string-append "http://" uri-or-str))]
	  [else (error "Malformed uri" uri-or-str)])))

(define (uri-path-string uri)
  (uri->string (make-uri #:path (uri-path uri))))

(define (header-ref header key)
  (alist-ref key header (lambda (x y)
			  (string-ci= (->string x)
				      (->string y)))))

(define (header-update header key value)
  (alist-update key value header (lambda (x y)
				   (string-ci= (->string x)
					       (->string y)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; send request
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (display-header header-alst #!optional (out (current-output-port)))
  (define (header-capitalize str-or-sym)
    (irregex-replace/all '(: bow ($ any)) (->string str-or-sym)
			 (lambda (m)
			   (string-upcase (irregex-match-substring m 1)))))
  (with-output-to-port out
    (lambda ()
      (for-each (lambda (lst)
                  (printf "~A: " (header-capitalize (car lst)))
                  (if (list? (cdr lst))
                      (for-each (cut printf "~A " <>) (cdr lst))
                      (printf "~A" (cdr lst)))
                  (newline))
                header-alst)
      (newline))))

(define (display-debug-header header-alst #!optional (out (current-output-port)))
  (with-output-to-port out
    (lambda ()
      (for-each (lambda (lst)
                  (printf "> ~A: " (car lst))
                  (if (list? (cdr lst))
                      (for-each (cut printf "~A " <>) (cdr lst))
                      (printf "~A" (cdr lst)))
                  (newline))
                header-alst)
      (display ">\n"))))

(define (display-request-line method path-str)
  (printf "~A ~A HTTP/1.1~%" (string-upcase (->string method)) path-str))

(define (send-request path-str header-alst body
                      #!key
                      (port (current-output-port))
                      method)
  (when (http-read-debug)
    (with-output-to-port (current-error-port)
      (lambda ()
	(display "> ") (display-request-line method path-str)
	(display-debug-header header-alst)
	(display "> ") (display body)
	(newline))))
  (with-output-to-port port
    (lambda ()
      (display-request-line method path-str)
      (display-header header-alst)
      (display body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; read response
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (parse-line line)
  (let ([m (irregex-match (irregex "([^:]+):\\s*(.+)" 'i) line)])
    (if m (cons (string->symbol (string-downcase (irregex-match-substring m 1)))
                (string-trim-both (irregex-match-substring m 2)))
        #f)))

(define (parse-status-line line)
  (let ([mch (irregex-match ".+?\\s+(\\d+)\\s+(.+)"
			    (string-trim-both line))])
    (if mch (cons (string->number (irregex-match-substring mch 1))
		  (irregex-match-substring mch 2))
	#f)))

;;; ->alist
(define (read-header #!optional (in (current-input-port)))
  (let loop ([line (read-line in)]
             [acc '()])
    (if (or (irregex-match '(* space) line) (eof-object? line))
        (reverse! acc)
        (let ([m (parse-line line)])
          (loop (read-line in)
                (if m (cons m acc)
		    (cons (cons 'status (parse-status-line line)) acc)))))))

(define (read-body header-alst #!optional (in (current-input-port)))
  (cond   
   ;; transfer-encoding: chunked
   [(and (alist-ref 'transfer-encoding header-alst)
         (string-ci= (alist-ref 'transfer-encoding header-alst) "chunked"))
    (read-chunked-body in)]
   ;; content-length
   [(alist-ref 'content-length header-alst) =>
    (lambda (len-str)
      (read-string (or (string->number len-str) 0) in))]
   [else ""]))

(define (read-chunked-body #!optional (in (current-input-port)))
  (define (inner)
    (let* ([line (read-line in)]
           [num (string->number line 16)])
      (when (> num 0)
        (display (read-string num in))
        (read-line in)                  ; eat newline
        (inner))))
  (with-output-to-string inner))

(defstruct response status header body)

(define (get-response #!optional (in (current-input-port)) (head? #f))
  (let* ([header-alst (read-header in)]
         [status (alist-ref 'status header-alst)]         
         [header-alst (alist-delete 'status header-alst)]
         [body (if head? #f (read-body header-alst in))])
    (make-response #:status status
		   #:header header-alst
		   #:body body)))
