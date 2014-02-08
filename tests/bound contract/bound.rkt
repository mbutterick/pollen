#lang racket

(provide bound/c (rename-out (top~ #%top)))

(define-syntax-rule (top~ . id)
   (λ x `(id ,@x)))

(define-syntax (bound/c stx)
   (syntax-case stx ()
     [(_ x)
      (if (identifier-binding #'x )
          #'x
          #'(#%top . x))]))
