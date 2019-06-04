#lang racket/base
(require pollen/setup)
(provide (except-out (all-from-out racket/base pollen/setup) #%module-begin)
         (rename-out [mb #%module-begin]) #%top-interaction)

(define-syntax-rule (mb MODE . ARGS)
  (#%module-begin
   (require (except-in "private/main-base.rkt" #%module-begin)
            (prefix-in p: (only-in "private/main-base.rkt" #%module-begin)))
   (provide (all-from-out "private/main-base.rkt")
            (rename-out [mb #%module-begin]))
   (define-syntax-rule (mb . OTHER-ARGS)
     (p:#%module-begin MODE . OTHER-ARGS))
   . ARGS))