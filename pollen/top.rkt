#lang racket/base
(require (for-syntax racket/base) pollen/tag)
(provide def/c (rename-out (top~ #%top)))

(define-syntax-rule (top~ . ID)
  (#%app make-default-tag-function 'ID))

(define-syntax (def/c stx)
  (syntax-case stx ()
    [(_ X) (identifier-binding #'X) #'X]
    [(_ X) #'(#%top . X)]))