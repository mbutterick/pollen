#lang racket/base
(require (prefix-in html: css-tools/html) sugar/define txexpr)


;; initial set of block tags: from html
(define+provide project-block-tags 
  (make-parameter html:block-tags))


;; tags are inline unless they're registered as block tags.
(define+provide/contract (block-txexpr? x)
  (any/c . -> . boolean?)
  (and (txexpr? x) (member (get-tag x) (project-block-tags)) #t))


(define+provide/contract (register-block-tag tag)
  (txexpr-tag? . -> . void?)
  (project-block-tags (cons tag (project-block-tags))))
