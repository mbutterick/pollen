#lang racket/base
(require pollen/setup)
(provide (except-out (all-from-out racket/base pollen/setup) #%module-begin)
         (rename-out [mb #%module-begin]) #%top-interaction)

(define-syntax-rule (mb mode . args)
  (#%module-begin
   (require (prefix-in p: "private/main-base.rkt"))
   (provide (rename-out [mb #%module-begin]))
   (define-syntax-rule (mb . other-args)
     (p:#%module-begin mode . other-args))
   . args))