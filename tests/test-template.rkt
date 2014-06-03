#lang racket/base
(require rackunit)
(require "../template.rkt")

(define x '(root (p "hello")))

(check-equal? (->html x) "<root><p>hello</p></root>")
(check-equal? (->html #:tag 'brennan x) "<brennan><p>hello</p></brennan>")
(check-equal? (->html #:attrs '((id "dale")) x) "<root id=\"dale\"><p>hello</p></root>")
(check-equal? (->html #:splice #t x) "<p>hello</p>")
(check-equal? (->html #:tag 'brennan #:attrs '((id "dale")) x) "<brennan id=\"dale\"><p>hello</p></brennan>")
(check-equal? (->html #:tag 'brennan #:attrs '((id "dale")) #:splice #t x) "<p>hello</p>")