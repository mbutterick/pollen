#lang racket/base
(provide (all-defined-out))

(define (split-metas tree meta-key)
  (define matches null)
  
  (define (meta? x)  ; meta has form (define-meta key value)
    (and (list? x) (>= (length x) 3) (eq? (car x) meta-key)))
  
  (define (non-meta?/gather x)
    (or (not (meta? x))
        (and (set! matches (cons x matches)) #f)))
  
  (define rest
    (let loop ([x (if (list? tree) tree (list tree))])
      (if (list? x)
          (map loop (filter non-meta?/gather x))
          x)))
  
  (let ([meta-key cadr][meta-value caddr])
    (values (map meta-key matches) (map meta-value matches) rest)))