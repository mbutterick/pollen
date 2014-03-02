#lang racket/base
(require xml txexpr sugar/define)
(require "predicates.rkt" "decode/typography.rkt")

(provide (all-from-out "decode/typography.rkt"))


(define+provide (to-string x)
  (if (string? x)
      x ; fast exit for strings
      (with-handlers  ([exn:fail? (λ(exn) (error (format "Pollen parser: can't convert ~v to ~a" x 'string)))])
        (cond
          [(equal? '() x) ""]
          [(symbol? x) (symbol->string x)]
          [(number? x) (number->string x)]
          [(path? x) (path->string x)]
          [(char? x) (format "~a" x)]
          [else (error)])))) ; put this last so other xexprish things don't get caught

;; add a block tag to the list
(define+provide/contract (register-block-tag tag)
  (symbol? . -> . void?)
  (append-block-tag tag))


;; decoder wireframe
(define+provide/contract (decode nx
                                 #:exclude-xexpr-tags [excluded-xexpr-tags '()]
                                 #:xexpr-tag-proc [xexpr-tag-proc (λ(x)x)]
                                 #:xexpr-attrs-proc [xexpr-attrs-proc (λ(x)x)]
                                 #:xexpr-elements-proc [xexpr-elements-proc (λ(x)x)]
                                 #:block-xexpr-proc [block-xexpr-proc (λ(x)x)]
                                 #:inline-xexpr-proc [inline-xexpr-proc (λ(x)x)]
                                 #:string-proc [string-proc (λ(x)x)])
  ((xexpr/c)  
   (#:exclude-xexpr-tags list?  
                         #:xexpr-tag-proc procedure?
                         #:xexpr-attrs-proc procedure?
                         #:xexpr-elements-proc procedure?
                         #:block-xexpr-proc procedure?
                         #:inline-xexpr-proc procedure?
                         #:string-proc procedure?) . ->* . txexpr?)
  

  (let loop ([x (validate-txexpr? nx)])
    (cond
      [(txexpr? x) (let-values([(tag attr elements) (txexpr->values x)]) 
                     (if (member tag excluded-xexpr-tags)    
                         x ; let x pass through untouched
                         (let ([decoded-xexpr (apply make-txexpr (map loop (list tag attr elements)))])
                           ((if (block-xexpr? decoded-xexpr)
                                block-xexpr-proc
                                inline-xexpr-proc) decoded-xexpr))))]
      [(txexpr-tag? x) (xexpr-tag-proc x)]
      [(txexpr-attrs? x) (xexpr-attrs-proc x)]
      ;; need this for operations that may depend on context in list
      [(txexpr-elements? x) (map loop (xexpr-elements-proc x))]
      [(string? x) (string-proc x)]
      ;; if something has made it through undecoded, that's a problem
      [else (error "decode: can't decode" x)])))

