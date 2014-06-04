#lang racket/base
(require rackunit)
(require "../template.rkt")

(define tx '(root (p "hello")))

(check-equal? (->html tx) "<root><p>hello</p></root>")
(check-equal? (->html #:tag 'brennan tx) "<brennan><p>hello</p></brennan>")
(check-equal? (->html #:attrs '((id "dale")) tx) "<root id=\"dale\"><p>hello</p></root>")
(check-equal? (->html #:splice #t tx) "<p>hello</p>")
(check-equal? (->html #:tag 'brennan #:attrs '((id "dale")) tx) "<brennan id=\"dale\"><p>hello</p></brennan>")
(check-equal? (->html #:tag 'brennan #:attrs '((id "dale")) #:splice #t tx) "<p>hello</p>")

(define x "hello")

(check-equal? (->html x) "hello")
(check-equal? (->html #:tag 'brennan x) "<brennan>hello</brennan>")
(check-exn exn:fail? (λ() (->html #:attrs '((id "dale")) x) "hello")) ;; won't work without tag
(check-equal? (->html #:splice #t x) "hello")
(check-equal? (->html #:tag 'brennan #:attrs '((id "dale")) x) "<brennan id=\"dale\">hello</brennan>")
(check-equal? (->html #:tag 'brennan #:attrs '((id "dale")) #:splice #t x) "hello")
