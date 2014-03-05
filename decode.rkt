#lang racket/base
(require xml txexpr sugar/define)
(require "predicates.rkt" "decode/typography.rkt" "debug.rkt")

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
(define+provide/contract (decode txexpr
                                 #:exclude-xexpr-tags [excluded-xexpr-tags '()]
                                 #:xexpr-tag-proc [xexpr-tag-proc (λ(x)x)]
                                 #:xexpr-attrs-proc [xexpr-attrs-proc (λ(x)x)]
                                 #:xexpr-elements-proc [xexpr-elements-proc (λ(x)x)]
                                 #:block-xexpr-proc [block-xexpr-proc (λ(x)x)]
                                 #:inline-xexpr-proc [inline-xexpr-proc (λ(x)x)]
                                 #:string-proc [string-proc (λ(x)x)]
                                 #:symbol-proc [symbol-proc (λ(x)x)]
                                 #:valid-char-proc [valid-char-proc (λ(x)x)]
                                 #:cdata-proc [cdata-proc (λ(x)x)])
  ((xexpr/c)  
   (#:exclude-xexpr-tags list?  
                         #:xexpr-tag-proc procedure?
                         #:xexpr-attrs-proc procedure?
                         #:xexpr-elements-proc procedure?
                         #:block-xexpr-proc procedure?
                         #:inline-xexpr-proc procedure?
                         #:string-proc procedure?
                         #:symbol-proc procedure?
                         #:valid-char-proc procedure?
                         #:cdata-proc procedure?) . ->* . txexpr?)
  

  (let loop ([x txexpr])
    (cond
      [(txexpr? x) (let-values([(tag attrs elements) (txexpr->values x)]) 
                     (if (member tag excluded-xexpr-tags)    
                         x ; because it's excluded
                         
                         ;; we apply processing here rather than do recursive descent on the pieces
                         ;; because if we send them back through loop, certain element types are ambiguous
                         ;; e.g., ((p "foo")) tests out as both txexpr-attrs and txexpr-elements
                         (let ([decoded-xexpr 
                                (apply make-txexpr (list (xexpr-tag-proc tag) 
                                                         (xexpr-attrs-proc attrs) 
                                                         (map loop (xexpr-elements-proc elements))))])
                           ((if (block-xexpr? decoded-xexpr)
                                block-xexpr-proc
                                inline-xexpr-proc) decoded-xexpr))))]
      [(string? x) (string-proc x)]
      [(symbol? x) (symbol-proc x)]
      [(valid-char? x) (valid-char-proc x)]
      [(cdata? x) (cdata-proc x)]
      [else (error "decode: can't decode" x)])))

