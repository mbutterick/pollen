#lang racket/base
(require (for-syntax racket/base) pollen/tag)
(provide def/c (rename-out (top~ #%top)))

;; Changes the default behavior of #%top.
;; Unbound identifiers are allowed, and treated as the 
;; tag in a txexpr (with the rest of the expression treated as the body)
;; To suppress this behavior, use def/c to wrap any name.
;; If that name isn't already defined, you'll get the usual syntax error.

(define-syntax-rule (top~ . id)
  (make-default-tag-function 'id))

(define-syntax (def/c stx)
  (syntax-case stx ()
    [(_ x)
     (if (identifier-binding #'x )
         #'x
         #'(#%top . x))]))