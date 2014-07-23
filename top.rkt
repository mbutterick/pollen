#lang racket/base

;; Changes the default behavior of #%top.
;; Unbound identifiers are allowed, and treated as the 
;; tag in a txexpr (with the rest of the expression treated as the body)
;; To suppress this behavior, use def/c to wrap any name.
;; If that name isn't already defined, you'll get the usual syntax error.

(require (for-syntax racket/base) pollen/tag)

(provide (except-out (all-defined-out) top~)
         (rename-out (top~ #%top)))

(define-syntax (top~ stx)
  (syntax-case stx ()
    [(_ . id) #'(make-tag-function 'id)]))

(define-syntax (def/c stx)
  (syntax-case stx ()
    [(_ x)
     (if (identifier-binding #'x )
         #'x
         #'(#%top . x))]))