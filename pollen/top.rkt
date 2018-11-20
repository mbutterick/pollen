#lang racket/base
(require (for-syntax racket/base pollen/setup) pollen/tag)
(provide def/c (rename-out (top~ #%top)))

(define-syntax (top~ stx)
  (syntax-case stx ()
    [(_ . ID)
     (setup:racket-style-top)
     #'(#%top . ID)]
    [(_ . ID)
     #'(#%app make-default-tag-function 'ID)]))

(define-syntax (def/c stx)
  (syntax-case stx ()
    [(_ X) (identifier-binding #'X) #'X]
    [(_ X) #'(#%top . X)]))