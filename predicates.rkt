#lang racket/base
(require racket/contract racket/match racket/set)
(require sugar txexpr)
(require "world.rkt" "file-tools.rkt" "debug.rkt")

(provide (all-from-out "file-tools.rkt"))

;; test for well-formed meta
(define+provide/contract (meta-xexpr? x)
  (any/c . -> . boolean?)
  (match x
    [`(meta ,(? string? key) ,(? string? value)) #t]
    [else #f]))

