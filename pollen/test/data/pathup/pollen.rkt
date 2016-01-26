#lang racket/base
(provide (all-defined-out))

(define (root . xs)
  `(one ,@xs))

(define (puppy)
  "one")