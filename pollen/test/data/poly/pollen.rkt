#lang racket/base
(require pollen/setup)
(provide (all-defined-out))

(module setup racket/base
  (provide (all-defined-out))
  (define poly-targets '(html txt))
  (define compile-cache-active #f))

(define (heading . xs)
  (case (setup:current-poly-target)
    [(txt) (map string-upcase xs)]
    [else `(h2 ,@xs)]))

(define (emph . xs)
  (case (setup:current-poly-target)
    [(txt) `("**" ,@xs "**")]
    [else `(strong ,@xs)]))