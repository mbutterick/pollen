#lang racket/base
(require xml txexpr sugar/define)
(require "decode/block.rkt" "decode/typography.rkt" "debug.rkt")

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


;; decoder wireframe
(define+provide/contract (decode txexpr
                                 #:txexpr-tag-proc [txexpr-tag-proc (λ(x)x)]
                                 #:txexpr-attrs-proc [txexpr-attrs-proc (λ(x)x)]
                                 #:txexpr-elements-proc [txexpr-elements-proc (λ(x)x)]
                                 #:block-txexpr-proc [block-txexpr-proc (λ(x)x)]
                                 #:inline-txexpr-proc [inline-txexpr-proc (λ(x)x)]
                                 #:string-proc [string-proc (λ(x)x)]
                                 #:symbol-proc [symbol-proc (λ(x)x)]
                                 #:valid-char-proc [valid-char-proc (λ(x)x)]
                                 #:cdata-proc [cdata-proc (λ(x)x)]
                                 
                                 #:exclude-tags [excluded-tags '()])
  ((xexpr/c)  
   (#:txexpr-tag-proc (txexpr-tag? . -> . txexpr-tag?)
                         #:txexpr-attrs-proc (txexpr-attrs? . -> . txexpr-attrs?)
                         #:txexpr-elements-proc (txexpr-elements? . -> . txexpr-elements?)
                         #:block-txexpr-proc (block-txexpr? . -> . block-txexpr?)
                         #:inline-txexpr-proc (txexpr? . -> . txexpr?)
                         #:string-proc (string? . -> . string?)
                         #:symbol-proc (symbol? . -> . symbol?)
                         #:valid-char-proc (valid-char? . -> . valid-char?)
                         #:cdata-proc (cdata? . -> . cdata?)
                         #:exclude-tags (listof symbol?)  ) . ->* . txexpr?)
  

  (let loop ([x txexpr])
    (cond
      [(txexpr? x) (let-values([(tag attrs elements) (txexpr->values x)]) 
                     (if (member tag excluded-tags)    
                         x ; because it's excluded
                         
                         ;; we apply processing here rather than do recursive descent on the pieces
                         ;; because if we send them back through loop, certain element types are ambiguous
                         ;; e.g., ((p "foo")) tests out as both txexpr-attrs and txexpr-elements
                         (let ([decoded-txexpr 
                                (apply make-txexpr (list (txexpr-tag-proc tag) 
                                                         (txexpr-attrs-proc attrs) 
                                                         (map loop (txexpr-elements-proc elements))))])
                           ((if (block-txexpr? decoded-txexpr)
                                block-txexpr-proc
                                inline-txexpr-proc) decoded-txexpr))))]
      [(string? x) (string-proc x)]
      [(symbol? x) (symbol-proc x)]
      [(valid-char? x) (valid-char-proc x)]
      [(cdata? x) (cdata-proc x)]
      [else (error "decode: can't decode" x)])))








