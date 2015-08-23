#lang racket/base
(require (for-syntax racket/base racket/syntax) sugar/test)
(provide (all-defined-out))

(define-syntax (define-caching-ns stx)
  (syntax-case stx ()
    [(_ name)
     #'(define-caching-ns name null)]
    [(_ name mods)
     (with-syntax ([caching-module-name (generate-temporary)])
       #'(begin
           (module caching-module-name racket/base
             (define-namespace-anchor nsa) ; could handle this macro-introduced name better
             (provide nsa))
           (require 'caching-module-name)
           (define name (namespace-anchor->namespace nsa))
           (require-in-namespace name mods)))]))

(define (require-in-namespace ns module-names)
  (parameterize ([current-namespace ns])
    (for-each (λ(mod-name) (namespace-require mod-name)) module-names)))

(define (attach-from-namespace from-ns to-ns module-names)
  (for-each (λ(mod-name) (namespace-attach-module from-ns mod-name to-ns)) module-names)
  (require-in-namespace to-ns module-names))


(module-test-external
 (define module-names '(xml racket/function))
 (define-caching-ns from-ns module-names)
 (check-true (eval '(andmap procedure? (list xexpr? curry)) from-ns))
 
 (define to-ns (make-base-namespace))
 (attach-from-namespace from-ns to-ns module-names)
 (check-true (eval '(andmap procedure? (list xexpr? curry)) to-ns)))



