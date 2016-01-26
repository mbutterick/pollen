#lang racket/base
(provide (all-defined-out))

(define (root . xs)
  `(two ,@xs))

(define (puppy)
  "two")