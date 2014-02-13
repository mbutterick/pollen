#lang racket/base

;; Changes the default behavior of #%top.
;; Unbound identifiers are allowed, and treated as the 
;; tag in a tagged-xexpr (with the rest of the expression treated as the body)
;; To suppress this behavior, use bound/c to wrap any name.
;; If that name isn't already defined, you'll get the usual syntax error.

(require (for-syntax racket/base))

(provide (except-out (all-defined-out) top~)
         (rename-out (top~ #%top)))

(define-syntax-rule (top~ . id)
  (λ x `(id ,@x)))

(define-syntax (bound/c stx)
   (syntax-case stx ()
     [(_ x)
      (if (identifier-binding #'x )
          #'x
          #'(#%top . x))]))
