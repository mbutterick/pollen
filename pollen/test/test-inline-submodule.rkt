#lang racket/base

(module inline pollen
  (define-meta foo 42)
  "Zut Alors")

(module test-main racket/base
  (require rackunit)
  (require (submod ".." inline))
  (check-equal? doc "Zut Alors")
  (check-equal? metas (hasheq 'foo 42)))

(module test-metas-submod racket/base
  (require rackunit)
  (require (submod ".." inline metas))
  (let ()
    (define-syntax-rule (#%top . xs)
      'unbound-identifier)
    (check-equal? doc 'unbound-identifier))
  (check-equal? metas (hasheq 'foo 42)))

(require 'test-main)
(require 'test-metas-submod)