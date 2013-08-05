#lang racket/base
(require racket/contract)
(require racket/list)
(require racket/string)

(module+ test (require rackunit))

(require "tools.rkt")
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


; Mon Aug 5: start here

; A block-xexpr is a named expression that's not on the inline list
; todo: bear in mind that browsers take the opposite view:
; that only elements on the block list are blocks
; and otherwise are treated as inline 
(define (block-xexpr? x)
  (and (named-xexpr? x) (not (in? inline-tags (car x)))))



; default content decoder for pollen
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
         (map &decode (if (any paragraph-break? x) ; need this condition to prevent infinite recursion
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
