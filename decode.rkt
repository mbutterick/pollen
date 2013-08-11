#lang racket/base
(require racket/contract racket/list racket/string racket/match)
(require (only-in racket/format ~a))
(require (only-in racket/bool nor))
(require (only-in xml xexpr/c))
(require (prefix-in scribble: (only-in scribble/decode whitespace?)))
(module+ test (require rackunit))

(require "tools.rkt" "library/html.rkt")
(provide (all-defined-out))


;; split list into list of sublists using test
;; todo: contract & unit tests
(define (splitf-at* pieces test)
  (define (splitf-at*-inner pieces [acc '()]) ; use acc for tail recursion
    (if (empty? pieces) 
        acc
        (let-values ([(item rest) 
                      (splitf-at (dropf pieces test) (compose1 not test))])
          (splitf-at*-inner rest `(,@acc ,item)))))
  (splitf-at*-inner (trim pieces test)))


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


;; todo: add native support for list-xexpr
;; decode triple newlines to list items

;; is the named-xexpr a block element (as opposed to inline)
(define/contract (block-xexpr? x)
  (any/c . -> . boolean?)
  ;; this is a change in behavior since first pollen
  ;; blocks are only the ones on the html block tag list.
  ;; todo: make sure this is what I want.
  ;; this is, however, more consistent with browser behavior
  ;; (browsers assume that tags are inline by default)
  ((named-xexpr? x) . and . (->boolean ((named-xexpr-name x) . in . block-tags))))

(module+ test
  (check-true (block-xexpr? '(p "foo")))
  (check-true (block-xexpr? '(div "foo")))
  (check-false (block-xexpr? '(em "foo")))
  (check-false (block-xexpr? '(barfoo "foo"))))


;; convert numbers to strings
;; maybe this isn't necessary
(define (stringify x)
  (map-tree (λ(i) (if (number? i) (->string i) i)) x))

(module+ test
  (check-equal? (stringify '(p 1 2 "foo" (em 4 "bar"))) '(p "1" "2" "foo" (em "4" "bar"))))



;; recursive whitespace test
;; Scribble's version misses whitespace in a list
(define (whitespace? x)
  (cond
    [(list? x) (andmap whitespace? x)]
    [else (scribble:whitespace? x)]))

(module+ test
  (check-false (scribble:whitespace? (list "\n" " " "\n"))) ; scribble result is too surprising
  (check-true (whitespace? " "))
  (check-false (whitespace? "foo"))
  (check-false (whitespace? " ")) ; a nonbreaking space
  (check-true (whitespace? "\n \n"))
  (check-true (whitespace? (list "\n" " " "\n")))
  (check-true (whitespace? (list "\n" " " "\n" (list "\n" "\n")))))



;; trim from beginning & end of list
(define (trim items test-proc)
  (list? procedure? . -> . list?)
  (dropf-right (dropf items test-proc) test-proc))

(module+ test
  (check-equal? (trim (list "\n" " " 1 2 3 "\n") whitespace?) '(1 2 3))
  (check-equal? (trim (list 1 3 2 4 5 6 8 9 13) odd?) '(2 4 5 6 8)))



;; test for well-formed meta
(define/contract (meta-xexpr? x)
  (any/c . -> . (λ(i) (or (boolean? i) (list? i))))
  (match x
    [`(meta ,(? string? key) ,(? string? value)) (list key value)]
    [else #f]))



;; function to strip metas (or any tag)
(define/contract (extract-tag-from-xexpr tag nx)
  (xexpr-name? named-xexpr? . -> . (values named-xexpr? xexpr-content?))
  (define matches '())
  (define (extract-tag x)
    (cond
      [(and (named-xexpr? x) (equal? tag (car x)))
       ; stash matched tag but return empty value
       (begin
         (set! matches (cons x matches))
         empty)]
      [(named-xexpr? x) (let-values([(name attr body) (break-named-xexpr x)]) 
                          (make-named-xexpr name attr (extract-tag body)))]
      [(xexpr-content? x) (filter-not empty? (map extract-tag x))]
      [else x]))
  (values (extract-tag nx) (reverse matches))) 


(module+ test
  (define x '(root (meta "foo" "bar") "hello" "world" (meta "foo2" "bar2") 
                   (em "goodnight" "moon" (meta "foo3" "bar3"))))
  
  (check-equal? (values->list (extract-tag-from-xexpr 'meta x)) 
                (list '(root "hello" "world" (em "goodnight" "moon"))
                      '((meta "foo" "bar") (meta "foo2" "bar2") (meta "foo3" "bar3")))))
;; decoder wireframe
(define/contract (decode nx
                         #:exclude-xexpr-names [excluded-xexpr-names '()]
                         #:xexpr-name-proc [xexpr-name-proc (λ(x)x)]
                         #:xexpr-attr-proc [xexpr-attr-proc (λ(x)x)]
                         #:xexpr-content-proc [xexpr-content-proc (λ(x)x)]
                         #:block-xexpr-proc [block-xexpr-proc (λ(x)x)]
                         #:inline-xexpr-proc [inline-xexpr-proc (λ(x)x)]
                         #:string-proc [string-proc (λ(x)x)]
                         #:meta-proc [meta-proc (λ(x)x)])
  ;; use xexpr/c for contract because it gives better error messages
  ((xexpr/c) (#:exclude-xexpr-names (λ(i) (or (symbol? i) (list? i)))
                                    #:xexpr-name-proc procedure?
                                    #:xexpr-attr-proc procedure?
                                    #:xexpr-content-proc procedure?
                                    #:block-xexpr-proc procedure?
                                    #:inline-xexpr-proc procedure?
                                    #:string-proc procedure?
                                    #:meta-proc procedure?)
             . ->* . named-xexpr?)
  (when (not (named-xexpr? nx))
    (error (format "decode: ~v not a full named-xexpr" nx)))
  
  (define (&decode x)
    (cond
      [(named-xexpr? x) (let-values([(name attr content) (break-named-xexpr x)]) 
                          (if (name . in . (->list excluded-xexpr-names))    
                              x
                              (let ([decoded-xexpr 
                                     (apply make-named-xexpr (map &decode (list name attr content)))])
                                ((if (block-xexpr? decoded-xexpr)
                                     block-xexpr-proc
                                     inline-xexpr-proc) decoded-xexpr))))]
      [(xexpr-name? x) (xexpr-name-proc x)]
      [(xexpr-attr? x) (xexpr-attr-proc x)]
      [(xexpr-content? x) (map &decode (xexpr-content-proc x))]
      [(string? x) (string-proc x)]
      [else x]))
  
  
  (let-values ([(nx metas) (extract-tag-from-xexpr 'meta nx)])
    (append (&decode nx) (map meta-proc metas))))