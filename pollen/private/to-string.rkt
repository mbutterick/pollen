#lang racket/base
(provide (all-defined-out))

(define (to-string x)
  (cond
    [(string? x) x]
    [(or (null? x) (void? x)) ""]
    [(or (symbol? x) (number? x) (path? x) (char? x)) (format "~a" x)]
    [else (format "~v" x)]))