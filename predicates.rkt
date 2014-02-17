#lang racket/base
(require racket/contract racket/match racket/set)
(require css-tools/html sugar tagged-xexpr)
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
  (xexpr-tag? . -> . void?)
  (set! project-block-tags (cons tag project-block-tags)))

;; is the tagged-xexpr a block element (as opposed to inline)
;; tags are inline unless they're registered as block tags.
(define+provide/contract (block-xexpr? x)
  (any/c . -> . boolean?)
  ;; (car x) = shorthand for tag of xexpr
  ((tagged-xexpr? x) . and . ((car x) . in? . project-block-tags)))


;; recursive whitespace test
(define+provide/contract (whitespace? x)
  (any/c . -> . boolean?)
  (cond
    [(or (vector? x) (list? x) (set? x)) (andmap whitespace? (->list x))]
    [(equal? "" x) #t] ; empty string is deemed whitespace
    [(or (symbol? x) (string? x)) (->boolean (regexp-match #px"^\\s+$" (->string x)))]
    [else #f]))




