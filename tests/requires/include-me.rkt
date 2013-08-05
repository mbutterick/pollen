#lang racket/base
(require racket/contract)
(require (planet mb/pollen/tools) (planet mb/pollen/decode))

(provide (all-defined-out))

(module+ test (require rackunit))

(define (root . items)
  (named-xexpr? . -> . named-xexpr?)
  `(root ,@(merge-newlines items)))

(module+ test
  (check-equal? (root "foo" "\n" "\n") '(root "foo" "\n\n")))

(define foo "bar")