#lang racket/base
(require racket/contract)
(require (only-in racket/list empty?))
(require (only-in racket/format ~a))

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
  (require rackunit)
  (check-equal? (as-string '()) "")
  (check-equal? (as-string 'foo) "foo")
  (check-equal? (as-string 123) "123")
  (define file-name-as-text "foo.txt")
  (check-equal? (as-string (string->path file-name-as-text)) file-name-as-text)
  (check-equal? (as-string #\¶) "¶")
  )