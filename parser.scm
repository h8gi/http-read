(use prcc)
(define (num-to-str start end)
  (apply string (map integer->char (iota (add1 (- end start)) start))))
(define p.tchar (<r> "[!#$%&'*+.^_`|~0-9a-zA-Z-]"))
(define p.vchar (one-of "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]\\^_`abcdefghijklmnopqrstuvwxyz{|}~"))

(define p.obs-text (one-of "€‚ƒ„…†‡ˆ‰Š‹Œ‘’“”•–—˜™š›œŸ ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏĞÑÒÓÔÕÖ×ØÙÚÛÜİŞßàáâãäåæçèéêëìíîïğñòóôõö÷øùúûüışÿ"))

(define p.token (<@> (<+> p.tchar)
		     (cut string-join <> "")))
(define p.token68 (<@> (<and> (<+> (<r> "[a-zA-Z0-9._~+/-]"))
			      (<*> (<c> #\=)))
		       (lambda (o)
			 (string-append (string-join (list-ref o 0) "")
					(string-join (list-ref o 1) "")))))

(define p.ows (<*> (<space>)))
(define p.bws p.ows)
(define p.method (<@> p.token
		      (compose string->symbol string-downcase)))

(define p.field-name p.token)

(define p.field-vchar p.vchar)
(define p.field-content (<and> p.field-vchar (<?> (pred (<+> (<space>)) p.field-vchar) )))
(define p.field-value (<@> (<*> p.field-content)
			   (compose (cut string-join <> "") flatten)))
;;; # 
(define (<+comma> elm)
  (<@> (<and> elm
	      (<*> (<#> (<and> p.ows (<s> ",") p.ows elm) 3)))
       (lambda (o)
	 (apply cons o))))
(define (<*comma> elm)
  (<?> (<+comma> elm)))


;;; header-field-value
;; quoted string
(define p.qdtext
  (<or> (<space>)
	(one-of "!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~")
	p.obs-text))
(define p.quoted-pair (<@> (<and> (<c> #\\)
				  (<or> (<space>) p.vchar p.obs-text))
			   (cut string-join <> "")))
(define p.quoted-string (<@> (<and> (<c> #\")
				    (<*>  (<or> p.qdtext
						p.quoted-pair))
				    (<c> #\"))
			     (lambda (o)
			       (string-join (list-ref o 1) ""))))
;; comment
(define p.ctext (<or> (<space>)
		      (one-of "!\"#$%&'*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~")
		      p.obs-text))
(define p.comment (<and> (<c> #\( )
			 (<*> (<or> p.ctext p.quoted-pair (lazy p.comment)))
			 (<c> #\) )))



(define p.request-line
  (<and_> p.method (<r> "[^ ]+") (<r> "[^ ]+")))

(define p.header-field
  (<@> (<and> p.field-name (<s> ":") p.ows p.field-value p.ows)
       (lambda (o)
	 (cons ((compose string->symbol string-downcase) (list-ref o 0))
	       (list-ref o 3)))))


;;; auth
(define p.auth-scheme (<@> p.token
			   (compose string->symbol string-downcase)))
(define p.auth-param (<@> (<and> p.token p.bws (<s> "=") p.bws (<or> p.token p.quoted-string))
			  (lambda (o)
			    (cons (list-ref o 0)
				  (list-ref o 4)))))
(define p.challenge (<@> (<and> p.auth-scheme
				(<?> (<#> (<and> (<+> (<space>))
						 (<or> (<+comma> p.auth-param)
						       p.token68)) 1)))
			 (cut apply cons <>)))
(define p.credentials p.challenge)
(define p.www-authenticate
  (<+comma> p.challenge))
(define p.authorization p.credentials)
