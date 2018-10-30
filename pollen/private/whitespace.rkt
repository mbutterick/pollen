#lang racket/base
(require racket/match)
(provide (all-defined-out))

(define (whitespace-base x #:nbsp-is-white? nbsp-white?)
  (define white-pat (pregexp (format "^[\\s~a]+$" (if nbsp-white? #\u00A0 ""))))
  (let loop ([x x])
    (match x
      ["" #true] ; empty string is deemed whitespace
      [(pregexp white-pat) #true] 
      [(? symbol?) (loop (symbol->string x))]
      [(? pair?) (andmap loop x)]
      [(? vector?) (loop (vector->list x))]
      [_ #false])))

(define (whitespace? x) (whitespace-base x #:nbsp-is-white? #f))

(define (not-whitespace? x) (not (whitespace? x)))

(define (whitespace/nbsp? x) (whitespace-base x #:nbsp-is-white? #t))

(module+ test
  (require rackunit racket/format)
  (check-true (whitespace? " "))
  (check-false (whitespace? (~a #\u00A0)))
  (check-true (whitespace/nbsp? (~a #\u00A0)))
  (check-true (whitespace/nbsp? (vector (~a #\u00A0))))
  (check-false (whitespace? (format " ~a " #\u00A0)))
  (check-true (whitespace/nbsp? (format " ~a " #\u00A0))))