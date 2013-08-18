#lang racket/base

(require (planet mb/pollen/pmap))

;
;(require "test-pmap.p")
;(require "pollen-lang-test.p")

(let ([left (make-page-sequence (main->pmap (dynamic-require "test.pmap" 'main)))]
      [right (make-page-sequence (main->pmap (dynamic-require "test-pmap.p" 'main)))])
  (print left)
  (print right)
  (andmap (λ(l r) (equal? l r)) left right))