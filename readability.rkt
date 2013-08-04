#lang racket/base
(require racket/contract)
(require (only-in racket/list empty?))
(require (only-in racket/format ~a))
(module+ test (require rackunit))

(provide (all-defined-out))

;; general way of coercing to string
(define/contract (as-string x)
  (any/c . -> . string?)
  (cond 
    [(empty? x) ""]
    [(symbol? x) (symbol->string x)]
    [(number? x) (number->string x)]
    [(path? x) (path->string x)]
    [(char? x) (~a x)]
    [else (error (format "Can't make ~a into string" x))]))

(module+ test
  (check-equal? (as-string '()) "")
  (check-equal? (as-string 'foo) "foo")
  (check-equal? (as-string 123) "123")
  (define file-name-as-text "foo.txt")
  (check-equal? (as-string (string->path file-name-as-text)) file-name-as-text)
  (check-equal? (as-string #\¶) "¶"))


;; general way of coercing to a list
(define (as-list x)
  (any/c . -> . list?)
  (cond 
    [(list? x) x]
    [(vector? x) (vector->list x)]
    [else (list x)])) 

(module+ test
  (check-equal? (as-list '(1 2 3)) '(1 2 3))
  (check-equal? (as-list (list->vector '(1 2 3))) '(1 2 3))
  (check-equal? (as-list "foo") (list "foo")))

;; general way of asking for length
(define (len x)
  (any/c . -> . integer?)
  (cond
    [(list? x) (length x)]
    [(string? x) (string-length x)]
    [(symbol? x) (len (as-string x))]
    [(vector? x) (vector-length x)]
    [(hash? x) (len (hash-keys x))]
    [else #f]))

(module+ test
  (check-equal? (len '(1 2 3)) 3)
  (check-not-equal? (len '(1 2)) 3) ; len 2
  (check-equal? (len "foo") 3)
  (check-not-equal? (len "fo") 3) ; len 2
  (check-equal? (len 'foo) 3)
  (check-not-equal? (len 'fo) 3) ; len 2
  (check-equal? (len (list->vector '(1 2 3))) 3)
  (check-not-equal? (len (list->vector '(1 2))) 3) ; len 2
  (check-equal? (len (make-hash '((a 1) (b 2) (c 3)))) 3)
  (check-not-equal? (len (make-hash '((a 1) (b 2) (b 3)))) 3) ; len 2

  )