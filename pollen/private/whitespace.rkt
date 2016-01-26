#lang racket/base
(provide (all-defined-out))

(define (whitespace-base x #:nbsp-is-white? nbsp-white?)
  (define pat (pregexp (format "^[\\s~a]+$" (if nbsp-white? #\u00A0 ""))))
  (and (let loop ([x x])
         (cond
           [(string? x) (or (zero? (string-length x)) (regexp-match pat x))] ; empty string is deemed whitespace
           [(symbol? x) (loop (symbol->string x))]
           [(pair? x) (andmap loop x)]
           [(vector? x) (loop (vector->list x))]
           [else #f]))
       #t))


(define (whitespace? x)
  (whitespace-base x #:nbsp-is-white? #f))


(define not-whitespace? (λ(x) (not (whitespace? x))))


(define (whitespace/nbsp? x)
  (whitespace-base x #:nbsp-is-white? #t))


(module+ test
 (require rackunit racket/format)
 (check-true (whitespace? " "))
 (check-false (whitespace? (~a #\u00A0)))
 (check-true (whitespace/nbsp? (~a #\u00A0)))
 (check-true (whitespace/nbsp? (vector (~a #\u00A0))))
 (check-false (whitespace? (format " ~a " #\u00A0)))
 (check-true (whitespace/nbsp? (format " ~a " #\u00A0))))