#lang racket

;; check that a `define-meta` is immediately available
(module metatest pollen
  (define-meta key "value")
  (define val (hash-ref metas 'key))
  (provide val))

(require rackunit 'metatest)
(check-equal? val "value")