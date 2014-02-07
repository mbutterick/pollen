#lang racket

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
