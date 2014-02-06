#lang racket/base
(require racket/contract racket/list racket/match)
(require pollen/tools pollen/decode
         pollen/library/decode-tools)

(provide (all-defined-out))

(module+ test (require rackunit))

;; register custom block tags
(register-block-tag 'bloq)
(register-block-tag 'fooble)


;; detect paragraphs
;; todo: unit tests
(define/contract (xexpr-elements-proc elements)
  (xexpr-elements? . -> . xexpr-elements?)
  (let ([elements (prep-paragraph-flow elements)]) 
    (if (ormap paragraph-break? elements) ; need this condition to prevent infinite recursion
        (map wrap-paragraph (splitf-at* elements paragraph-break?)) ; split into ¶¶
        elements)))


(define/contract (block-xexpr-proc bx)
  (tagged-xexpr? . -> . tagged-xexpr?)
  (wrap-hanging-quotes (nonbreaking-last-space bx)))



(define (string-proc str)
  (string? . -> . string?)
  (typogrify str))


(define/contract (root . items)
  (() #:rest (listof xexpr-element?) . ->* . tagged-xexpr?)
  (decode (cons 'root-function items)
          ;          #:exclude-xexpr-tags 'em
          ;          #:xexpr-tag-proc [xexpr-tag-proc (λ(x)x)]
          ;          #:xexpr-attr-proc [xexpr-attr-proc (λ(x)x)]
          #:xexpr-elements-proc xexpr-elements-proc
          #:block-xexpr-proc block-xexpr-proc
          ;          #:inline-xexpr-proc [inline-xexpr-proc (λ(x)x)]
          #:string-proc string-proc))


(define foo "bar")