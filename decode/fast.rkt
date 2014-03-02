#lang racket/base
(require racket/match xml)
(require "../tools.rkt" "../predicates.rkt" txexpr "typography-fast.rkt")


(provide (all-defined-out) (all-from-out "typography-fast.rkt"))

;; general way of coercing to string
(define (to-string x)
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
;; this function is among the predicates because it alters a predicate globally.
(define (register-block-tag tag)
  (append-block-tag tag))



;; decoder wireframe
(define (decode nx
                                 #:exclude-xexpr-tags [excluded-xexpr-tags '()]
                                 #:xexpr-tag-proc [xexpr-tag-proc (λ(x)x)]
                                 #:xexpr-attrs-proc [xexpr-attrs-proc (λ(x)x)]
                                 #:xexpr-elements-proc [xexpr-elements-proc (λ(x)x)]
                                 #:block-xexpr-proc [block-xexpr-proc (λ(x)x)]
                                 #:inline-xexpr-proc [inline-xexpr-proc (λ(x)x)]
                                 #:string-proc [string-proc (λ(x)x)])
  
  (when (not (txexpr? nx))
    (error (format "decode: ~v not a full txexpr" nx)))
  
  
  (define (&decode x)
    (cond
      [(txexpr? x) (let-values([(tag attr elements) (txexpr->values x)]) 
                     (if (member tag excluded-xexpr-tags)    
                         x ; let x pass through untouched
                         (let ([decoded-xexpr (apply make-txexpr 
                                                     (map &decode (list tag attr elements)))])
                           ((if (block-xexpr? decoded-xexpr)
                                block-xexpr-proc
                                inline-xexpr-proc) decoded-xexpr))))]
      [(txexpr-tag? x) (xexpr-tag-proc x)]
      [(txexpr-attrs? x) (xexpr-attrs-proc x)]
      ;; need this for operations that may depend on context in list
      [(txexpr-elements? x) (map &decode (xexpr-elements-proc x))]
      [(string? x) (string-proc x)]
      ;; if something has made it through undecoded, that's a problem
      [else (error "Can't decode" x)]))
  
  
  (&decode nx))

