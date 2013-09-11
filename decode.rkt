#lang racket/base
(require racket/contract)
(require (only-in xml xexpr/c))
(require "tools.rkt")

(module+ test (require rackunit))

(provide (all-defined-out))


;; decoder wireframe
(define/contract (decode nx
                         #:exclude-xexpr-tags [excluded-xexpr-tags '()]
                         #:xexpr-tag-proc [xexpr-tag-proc (λ(x)x)]
                         #:xexpr-attr-proc [xexpr-attr-proc (λ(x)x)]
                         #:xexpr-elements-proc [xexpr-elements-proc (λ(x)x)]
                         #:block-xexpr-proc [block-xexpr-proc (λ(x)x)]
                         #:inline-xexpr-proc [inline-xexpr-proc (λ(x)x)]
                         #:string-proc [string-proc (λ(x)x)])
  ((xexpr/c) ;; use xexpr/c for contract on nx because it gives better error messages
   
   ;; todo: how to write more specific contracts for these procedures?
   ;; e.g., string-proc should be restricted to procs that accept a string as input
   ;; and return a string as output
   (#:exclude-xexpr-tags list?
                         #:xexpr-tag-proc procedure?
                         #:xexpr-attr-proc procedure?
                         #:xexpr-elements-proc procedure?
                         #:block-xexpr-proc procedure?
                         #:inline-xexpr-proc procedure?
                         #:string-proc procedure?)
   . ->* . tagged-xexpr?)
  (when (not (tagged-xexpr? nx))
    (error (format "decode: ~v not a full tagged-xexpr" nx)))
  
  
  (define (&decode x)
    (cond
      [(tagged-xexpr? x) (let-values([(tag attr elements) (break-tagged-xexpr x)]) 
                           (if (tag . in? . excluded-xexpr-tags)    
                               x ; let x pass through untouched
                               (let ([decoded-xexpr (apply make-tagged-xexpr 
                                                           (map &decode (list tag attr elements)))])
                                 ((if (block-xexpr? decoded-xexpr)
                                      block-xexpr-proc
                                      inline-xexpr-proc) decoded-xexpr))))]
      [(xexpr-tag? x) (xexpr-tag-proc x)]
      [(xexpr-attr? x) (xexpr-attr-proc x)]
      ;; need this for operations that may depend on context in list
      [(xexpr-elements? x) (map &decode (xexpr-elements-proc x))]
      [(string? x) (string-proc x)]
      ;; if something has made it through undecoded, that's a problem
      [else (error "Can't decode" x)]))
  
  
  (&decode nx))
