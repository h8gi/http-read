(use tcp6 openssl uri-common)
(define client-debug (make-parameter #f))
(set! char-set:uri-unreserved
  (list->char-set '(#\- #\. #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\_ #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\~)))
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
                   (body (lambda () (display "")))
                   (header '())
                   (method "GET")
                   (query '()))
  (let ([uri (if (uri? uri-or-str) uri-or-str (uri-reference uri-or-str))])
    (receive (in out) (connect-to-server uri)
      (dynamic-wind
        (lambda ()
          (send-request (uri->string (make-uri #:path (uri-path uri)
                                               #:query query))
                        (cons (cons 'Host (uri-host uri)) header)
                        body
                        #:method method
                        #:port out))
        (lambda ()
          (get-response in))
        (lambda ()
          (close-input-port in)
          (close-output-port out))))))

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

(define (send-request path-str header-alst body-thunk
                      #!key
                      (port (current-output-port))
                      (method "GET"))
  (when (client-debug)
    (printf "~A ~A HTTP/1.1~%" method path-str)
    (display-header header-alst)
    (body-thunk))
  (with-output-to-port port
    (lambda ()
      (printf "~A ~A HTTP/1.1~%" method path-str)
      (display-header header-alst)
      (body-thunk))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; read response
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (parse-line line)
  (let ([m (irregex-match (irregex "([^:]+): *(.+)" 'i) line)])
    (if m (cons (string->symbol (string-downcase (irregex-match-substring m 1)))
                (string-trim-both (irregex-match-substring m 2)))
        #f)))

;;; ->alist
(define (process-header #!optional (in (current-input-port)))
  (let loop ([line (read-line in)]
             [acc '()])
    (if (or (irregex-match '(* space) line) (eof-object? line))
        (reverse! acc)
        (let ([m (parse-line line)])
          (loop (read-line in)
                (if m (cons m acc) (cons (cons 'first-line line) acc)))))))

(define (process-body header-alst #!optional (in (current-input-port)))
  (let* ([len (or (alist-ref 'content-length header-alst) "0")]
         [len (or (string->number len) 0)])
    (read-string len in)))

(define-record response header body)
(define (get-response #!optional (in (current-input-port)))
  (let* ([header-alst (process-header in)]
         [body (process-body header-alst in)])
    (make-response header-alst body)))
