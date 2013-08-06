#lang racket/base
(require racket/contract)
(require racket/list)
(require racket/string)
(require (only-in racket/format ~a))
(require (only-in racket/bool nor))
(require (only-in xml xexpr/c))
(module+ test (require rackunit))

(require "tools.rkt" "library/html.rkt")
(provide (all-defined-out))

;; Find adjacent newline characters in a list and merge them into one item
;; Scribble, by default, makes each newline a separate list item
;; In practice, this is worthless.
(define/contract (merge-newlines x)
  (list? . -> . list?)
  (define (newline? x)
    (and (string? x) (equal? "\n" x)))
  (define (not-newline? x)
    (not (newline? x)))
  
  (define (really-merge-newlines xs [acc '()])
    (if (empty? xs)
        acc
        ;; Try to peel the newlines off the front.
        (let-values ([(leading-newlines remainder) (splitf-at xs newline?)])
          (if (not (empty? leading-newlines)) ; if you got newlines ...
              ;; combine them into a string and append them to the accumulator, 
              ;; and recurse on the rest
              (really-merge-newlines remainder (append acc (list (string-join leading-newlines ""))))
              ;; otherwise peel off elements up to the next newline, append them to accumulator,
              ;; and recurse on the rest
              (really-merge-newlines (dropf remainder not-newline?) 
                                     (append acc (takef remainder not-newline?)))))))
  
  (cond
    [(list? x) (really-merge-newlines (map merge-newlines x))]
    [else x]))

(module+ test
  (check-equal? (merge-newlines '(p "\n" "foo" "\n" "\n" "bar" (em "\n" "\n" "\n"))) 
                '(p "\n" "foo" "\n\n" "bar" (em "\n\n\n"))))


;; is the named-xexpr a block element (as opposed to inline)
(define/contract (block-xexpr? nx)
  (named-xexpr? . -> . boolean?)
  ;; this is a change in behavior since first pollen
  ;; blocks are only the ones on the html block tag list.
  ;; todo: make sure this is what I want.
  ;; this is, however, more consistent with browser behavior
  ;; (browsers assume that tags are inline by default)
  (as-boolean (in block-tags (named-xexpr-name nx))))

(module+ test
  (check-true (block-xexpr? '(p "foo")))
  (check-true (block-xexpr? '(div "foo")))
  (check-false (block-xexpr? '(em "foo")))
  (check-false (block-xexpr? '(barfoo "foo"))))

;; start here Tues 6
;; default content decoder for pollen
(define/contract (decode nx)
  ;; use xexpr/c for contact because it gives better error messages
  (xexpr/c . -> . named-xexpr?)
  nx
  
  )

;(decode `(p ((key "value")) ,decode))

#|
;; default content decoder for pollen
(define/contract (decode x)
  (named-xexpr? . -> . named-xexpr?)
  
  (define (&decode x)
    (cond
      [(named-xexpr? x)
       (let-values([(name attr content) (break-named-xexpr x)]) 
         (define decoded-x (make-named-xexpr name attr (&decode content)))
         (if (block-xexpr? decoded-x)
             ; add nonbreaking-last-space to the next line when ready
             (wrap-hanging-quotes (nonbreaking-last-space decoded-x)) ; do special processing for block xexprs
             decoded-x))]
      [(xexpr-content? x) ; a list of xexprs
       (let ([x (prep-paragraph-flow x)]) 
         (map &decode (if (ormap paragraph-break? x) ; need this condition to prevent infinite recursion
                          (map wrap-paragraph (splitf-at* x paragraph-break?)) ; split into ¶¶
                          x)))]     
      [(string? x) (typogrify x)]
      [else x]))
  
  (define (stringify x) ; convert numbers to strings
    (cond
      [(list? x) (map stringify x)]
      [(number? x) (~a x)]
      [else x]))
  
  (let* ([x (stringify x)]
         [x (trim-whitespace x)])
    (if (named-xexpr? x)
        (&decode x)
        ;todo: improve this error message, more specific location
        ; now, it just spits out the whole defective content
        (error (format "decode: ~v not a full named-xexpr" x)))))
|#