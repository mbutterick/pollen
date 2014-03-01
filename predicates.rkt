#lang racket/base
(require racket/contract racket/match racket/set)
(require css-tools/html sugar txexpr)
(require "world.rkt" "file-tools.rkt" "debug.rkt")

(provide (all-from-out "file-tools.rkt"))

;; test for well-formed meta
(define+provide/contract (meta-xexpr? x)
  (any/c . -> . boolean?)
  (match x
    [`(meta ,(? string? key) ,(? string? value)) #t]
    [else #f]))

;; initial set of block tags: from html
(define project-block-tags block-tags)

(define+provide/contract (append-block-tag tag)
  (txexpr-tag? . -> . void?)
  (set! project-block-tags (cons tag project-block-tags)))

;; is the txexpr a block element (as opposed to inline)
;; tags are inline unless they're registered as block tags.
(define+provide/contract (block-xexpr? x)
  (any/c . -> . boolean?)
  ;; (car x) = shorthand for tag of xexpr
  ((txexpr? x) . and . ((car x) . in? . project-block-tags)))






