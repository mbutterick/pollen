#lang racket/base

(module provider pollen
  (define foo 42)
  "word")

(require rackunit 'provider)
(check-equal? foo 42)
(check-equal? metas (hasheq))
(check-equal? doc "word")