#lang racket/base
(require racket/runtime-path rackunit)

(module m pollen
  (require racket/runtime-path)
  (define-runtime-path x "x")
  (provide x))

(require (submod "." m))

(define-runtime-path y "x")

(check-equal? x y)
