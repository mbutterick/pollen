#lang racket/base
 
(require pollen/tag)
(provide (all-defined-out))
(define headline (make-default-tag-function 'h2))
(define items (make-default-tag-function 'ul))
(define item (make-default-tag-function 'li 'p))
(define (link url text) `(a [[href ,url]] ,text))

(module setup racket/base
  (provide (all-defined-out))
  (define compile-cache-active #f)
  (define render-cache-active #f))
