#lang racket/base
(require racket/contract racket/list)
(require (planet mb/pollen/tools) (planet mb/pollen/decode))

(provide (all-defined-out))

(module+ test (require rackunit))

(define (meta-proc meta)
  `(meta ((name ,(->string (second meta)))(content ,(->string (third meta))))))

(define (string-proc string)
  "puppies")

(define (xexpr-content-proc content)
  (map (λ(i) "boing") content))

(define (root . items)
  (named-xexpr? . -> . named-xexpr?)
  (decode (cons 'root items)
          #:exclude-xexpr-names 'em
;          #:xexpr-name-proc [xexpr-name-proc (λ(x)x)]
;          #:xexpr-attr-proc [xexpr-attr-proc (λ(x)x)]
 ;         #:xexpr-content-proc xexpr-content-proc
;          #:block-xexpr-proc [block-xexpr-proc (λ(x)x)]
;          #:inline-xexpr-proc [inline-xexpr-proc (λ(x)x)]
 ;         #:string-proc string-proc
 ;         #:meta-proc meta-proc
          ))


(define foo "bar")