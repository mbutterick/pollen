#lang racket/base
(require sugar/define sugar/test txexpr)

(define (trim-outer-tag html)
  (define matches (regexp-match #px"^<.*?>(.*)</.*?>$" html))
  (define paren-match (cadr matches))
  paren-match)


(define+provide/contract (->html x-arg #:tag [tag #f] #:attrs [attrs #f] #:splice? [splice? #f] #:splice [bwc-splice? #f])
  (((or/c txexpr-element? txexpr-elements?)) (#:tag (or/c #f txexpr-tag?) #:attrs (or/c #f txexpr-attrs?) #:splice? boolean? #:splice boolean?) . ->* . string?)
  
  (define x (cond
              [(txexpr? x-arg) x-arg]
              [(list? x-arg) (cons 'html x-arg)]
              [else x-arg]))
  
  (when (and (not (txexpr? x)) attrs (not tag))
    (raise-argument-error '->html "can't use attribute list without a #:tag argument" tag))
  
  (cond
    [(or tag (txexpr? x))
     (define html-tag (or tag (get-tag x)))
     (define html-attrs (or attrs (and (txexpr? x) (get-attrs x)) null))
     (define html-elements (or (and (txexpr? x) (get-elements x)) (list x)))
     (define html (xexpr->html (txexpr html-tag html-attrs html-elements)))
     (if (or splice? bwc-splice? (and (list? x-arg) (not (txexpr? x-arg)) (not tag)))
         (trim-outer-tag html)
         html)]
     [else (xexpr->html x)]))

(module-test-external
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
 (check-exn exn:fail? (λ() (->html #:attrs '((id "dale")) x) "hello")) ;; won't work without tag
 (check-equal? (->html #:splice? #t x) "hello")
 (check-equal? (->html #:tag 'brennan #:attrs '((id "dale")) x) "<brennan id=\"dale\">hello</brennan>")
 (check-equal? (->html #:tag 'brennan #:attrs '((id "dale")) #:splice? #t x) "hello")
 
 (define xs '("hello " (em "you") " " 42))
 (check-equal? (->html xs) "hello <em>you</em> &#42;")
 (check-equal? (->html #:splice? #t xs) "hello <em>you</em> &#42;")
 (check-equal? (->html #:tag 'div xs) "<div>hello <em>you</em> &#42;</div>"))
