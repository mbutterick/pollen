#lang racket/base
(provide get-language-info)
(define (get-language-info top-here-path)
  (λ(key default)
    (case key
      [(configure-runtime) `(#(pollen/runtime-config configure ,top-here-path))]
      [else default])))