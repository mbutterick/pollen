#lang racket/base
(require (for-syntax racket/base racket/syntax))
(provide (all-defined-out))

(define-syntax (define-caching-ns stx)
  (syntax-case stx ()
    [(_ name)
     (with-syntax ([caching-module-name (generate-temporary)]
                   [NS-NAME (format-id stx "~a" #'name)])
       #'(begin
           (module caching-module-name racket/base
             (define-namespace-anchor nsa) ; could handle this macro-introduced name better
             (provide nsa))
           (require 'caching-module-name)
           (define NS-NAME (namespace-anchor->namespace nsa))))]))


(define (load-in-namespace to-ns . module-names)
  (for-each (λ(mn) (eval `(require ,mn) to-ns)) module-names))

(define (copy-from-namespace from-ns to-ns . module-names)
  (for-each (λ(mn) (namespace-attach-module from-ns mn to-ns)) module-names))
