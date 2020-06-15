#lang racket/base
(require sugar/define sugar/test txexpr/base pollen/private/splice pollen/setup)

(define (trim-outer-tag html)
  (define matches (regexp-match #px"^<.*?>(.*)</.*?>$" html))
  (define paren-match (cadr matches))
  paren-match)

(define (has-outer-splice-tag? x)
  (and (pair? x) (eq? (get-tag x) pollen-splicing-tag)))

(define+provide/contract (->html x-arg-maybe-spliced
                                 #:tag [tag #f]
                                 #:attrs [attrs #f]
                                 #:splice? [new-splice-arg? #f]
                                 #:splice [backward-compatible-splice-arg? #f])
  (((or/c txexpr-element? txexpr-elements?))
   (#:tag (or/c #f txexpr-tag?)
    #:attrs (or/c #f txexpr-attrs?)
    #:splice? boolean?
    #:splice boolean?) . ->* . string?)

  ;; handle an outer splice tag specially, because `splice` will leave it
  (define x-arg (if (has-outer-splice-tag? x-arg-maybe-spliced)
                    (cdr x-arg-maybe-spliced)
                    x-arg-maybe-spliced))

  ;; x is an X-expression
  (define x (if (list? x-arg)
                (splice (if (txexpr? x-arg)
                            x-arg
                            (cons 'html x-arg)) pollen-splicing-tag) ; list of txexpr-elements
                x-arg))
  
  (when (and (not (txexpr? x)) attrs (not tag))
    (raise-argument-error '->html "can't use attribute list without a #:tag argument" tag))

  (define splice? (or new-splice-arg? backward-compatible-splice-arg?))
  (cond
    [(or tag (txexpr? x))
     (define html-tag (or tag (get-tag x)))
     (define html-attrs (or attrs (and (txexpr? x) (get-attrs x)) null))
     (define html-elements (or (and (txexpr? x) (get-elements x)) (list x)))
     (define html (xexpr->html (txexpr html-tag html-attrs html-elements)))
     (if (or splice? (and (list? x-arg) (not (txexpr? x-arg)) (not tag)))
         (trim-outer-tag html)
         html)]
    [else (xexpr->html x)]))

(module-test-external
 (require txexpr)
 (define tx '(root (p "hello")))
 (check-equal? (->html tx) "<root><p>hello</p></root>")
 (check-equal? (->html #:tag 'brennan tx) "<brennan><p>hello</p></brennan>")
 (check-equal? (->html #:attrs '((id "dale")) tx) "<root id=\"dale\"><p>hello</p></root>")
 (check-equal? (->html #:splice? #t tx) "<p>hello</p>")
 (check-equal? (->html #:tag 'brennan #:attrs '((id "dale")) tx) "<brennan id=\"dale\"><p>hello</p></brennan>")
 (check-equal? (->html #:tag 'brennan #:attrs '((id "dale")) #:splice #t tx) "<p>hello</p>")
 (define x "hello")
 (check-equal? (->html x) "hello")
 (check-equal? (->html #:tag 'brennan x) "<brennan>hello</brennan>")
 (check-exn exn:fail? (λ () (->html #:attrs '((id "dale")) x) "hello")) ;; won't work without tag
 (check-equal? (->html #:splice? #t x) "hello")
 (check-equal? (->html #:tag 'brennan #:attrs '((id "dale")) x) "<brennan id=\"dale\">hello</brennan>")
 (check-equal? (->html #:tag 'brennan #:attrs '((id "dale")) #:splice? #t x) "hello")
 
 (define xs '("hello " (em "you") " " 42))
 (check-equal? (->html xs) "hello <em>you</em> &#42;")
 (check-equal? (->html #:splice? #t xs) "hello <em>you</em> &#42;")
 (check-equal? (->html #:tag 'div xs) "<div>hello <em>you</em> &#42;</div>")
 (check-equal? (->html '(@ "Markup in " (@ "italic"))) "Markup in italic")
 (check-equal? (->html '("Markup in " (@ "italic"))) "Markup in italic")
 (check-equal? (->html `(div "<![CDATA[Foo < Bar]]>")) "<div><![CDATA[Foo < Bar]]></div>"))
