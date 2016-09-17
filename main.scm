(use tcp6 openssl uri-common)
(define http-read-debug (make-parameter #f))
(set! char-set:uri-unreserved
  (string->char-set "-.0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~"))
;;; uri = (uri-reference str)
(define (connect-to-server uri)
  (let ([host (uri-host uri)]
        [scheme (uri-scheme uri)]
        [port (uri-port uri)])
    (receive (i o)
        (case scheme
          [(http) (tcp-connect host port)]
          [(https) (ssl-connect host port)])
      (if (and i o) (values i o)
          (error 'connect-to-server uri)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; all
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (http-read uri-or-str 
                   #!key
                   (header '())
                   (method 'get)
                   (query '()))
  (let* ([uri (if (uri? uri-or-str)
		  uri-or-str
		  (uri-reference uri-or-str))]
	 [uri (if (and (not (uri-scheme uri)) (string? uri-or-str))
		  (uri-reference (string-append "http://" uri-or-str))
		  uri)]
         [path (uri->string (make-uri #:path (uri-path uri)))]
         [query (append (uri-query uri) query)]
         [path (if (or (null? query) (eq? method 'post)) path
                   (string-append path
                                  "?"
                                  (form-urlencode query #:separator (char-set #\&))))]
	 [body (if (eq? method 'post)
		   (form-urlencode query #:separator (char-set #\&))
		   "")]
	 [content-length (string-length body)]
	 [header (if (eq? method 'post)
		     (alist-update 'content-length content-length header)
		     header)]
	 [header (if (header-ref 'host header)
		     header
		     (alist-update 'host (uri-host uri) header))]
	 [header (if (header-ref 'user-agent header)
		     header
		     (alist-update 'user-agent "http-read" header))])
    (receive (in out) (connect-to-server uri)
      (dynamic-wind
        (lambda ()
          (send-request path
                        header
                        body
                        #:method method
                        #:port out))
        (lambda ()
          (get-response in))
        (lambda ()
          (close-input-port in)
          (close-output-port out))))))

(define (header-ref key header)
  (alist-ref key header (lambda (x y)
			  (string-ci= (->string x)
				      (->string y)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; send request
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (display-header header-alst #!optional (out (current-output-port)))
  (with-output-to-port out
    (lambda ()
      (for-each (lambda (lst)
                  (printf "~A: " (car lst))
                  (if (list? (cdr lst))
                      (for-each (cut printf "~A " <>) (cdr lst))
                      (printf "~A" (cdr lst)))
                  (newline))
                header-alst)
      (newline))))

(define (send-request path-str header-alst body
                      #!key
                      (port (current-output-port))
                      method)
  (when (http-read-debug)
    (printf "~A ~A HTTP/1.1~%" method path-str)
    (display-header header-alst)
    (display body))
  (with-output-to-port port
    (lambda ()
      (printf "~A ~A HTTP/1.1~%" method path-str)
      (display-header header-alst)
      (display body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; read response
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (parse-line line)
  (let ([m (irregex-match (irregex "([^:]+): *(.+)" 'i) line)])
    (if m (cons (string->symbol (string-downcase (irregex-match-substring m 1)))
                (string-trim-both (irregex-match-substring m 2)))
        #f)))

;;; ->alist
(define (read-header #!optional (in (current-input-port)))
  (let loop ([line (read-line in)]
             [acc '()])
    (if (or (irregex-match '(* space) line) (eof-object? line))
        (reverse! acc)
        (let ([m (parse-line line)])
          (loop (read-line in)
                (if m (cons m acc) (cons (cons 'status line) acc)))))))

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

(define-record response status header body)

(define (get-response #!optional (in (current-input-port)))
  (let* ([header-alst (read-header in)]
         [status (alist-ref 'status header-alst)]
         [status (string-trim-both status)]
         [mch (irregex-match  ".+?[[:space:]]+(\\d+)[[:space:]]+(.+)" status)]
         [status-num (if mch (string->number (irregex-match-substring mch 1)) #f)]
         [status-sym (if mch (irregex-match-substring mch 2) "")]
         [status (cons status-num status-sym)]
         [header-alst (alist-delete 'status header-alst)]
         [body (read-body header-alst in)])
    (make-response status header-alst body)))
