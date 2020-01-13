#lang racket

;; check that a `define-meta` is immediately available
(module metatest pollen
  (define-meta key "value")
  (define val (hash-ref metas 'key))
  (provide val))

(require rackunit 'metatest)
(check-equal? val "value")

;; check that exported metas are a copy of final state of current-metas
(module metatest2 pollen
  (define-meta key "value")
  (define (tag . xs)
    (current-metas (hash-set (current-metas) 'key "reset"))
    "")
  (tag "hello")
  (define val (hash-ref metas 'key))
  (provide val))

(require (prefix-in 2: 'metatest2))
(check-equal? val "value")
(check-equal? (hash-ref 2:metas 'key) "reset")