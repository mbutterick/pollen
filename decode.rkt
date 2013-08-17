#lang racket/base
(require racket/contract racket/list racket/string racket/match)
(require (only-in racket/format ~a))
(require (only-in racket/bool nor))
(require (only-in xml xexpr/c))
(module+ test (require rackunit))

(require "tools.rkt")
(provide (all-defined-out))

;; split list into list of sublists using test-proc
(define/contract (splitf-at* xs split-test)
  ;; todo: better error message when split-test is not a predicate
  (list? predicate/c . -> . (listof list?))
  (define (&splitf-at* xs [acc '()]) ; use acc for tail recursion
    (if (empty? xs) 
        ;; reverse because accumulation is happening backward 
        ;; (because I'm using cons to push latest match onto front of list)
        (reverse acc)
        (let-values ([(item rest) 
                      ;; drop matching elements from front
                      ;; then split on nonmatching 
                      ;; = nonmatching item + other elements (which will start with matching)
                      (splitf-at (dropf xs split-test) (compose1 not split-test))])
          ;; recurse, and store new item in accumulator
          (&splitf-at* rest (cons item acc)))))
  
  ;; trim off elements matching split-test
  (&splitf-at* (trim xs split-test)))

(module+ test
  (check-equal? (splitf-at* '(1 2 3 4 5 6) even?) '((1)(3)(5)))
  (check-equal? (splitf-at* '("foo" " " "bar" "\n" "\n" "ino") whitespace?) '(("foo")("bar")("ino"))))



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


;; convert numbers to strings
;; maybe this isn't necessary
(define (stringify x)
  (map-tree (λ(i) (if (number? i) (->string i) i)) x))

(module+ test
  (check-equal? (stringify '(p 1 2 "foo" (em 4 "bar"))) '(p "1" "2" "foo" (em "4" "bar"))))


;; trim from beginning & end of list
(define (trim items test-proc)
  (list? procedure? . -> . list?)
  (dropf-right (dropf items test-proc) test-proc))

(module+ test
  (check-equal? (trim (list "\n" " " 1 2 3 "\n") whitespace?) '(1 2 3))
  (check-equal? (trim (list 1 3 2 4 5 6 8 9 13) odd?) '(2 4 5 6 8)))


;; decoder wireframe
(define/contract (decode nx
                         #:exclude-xexpr-tags [excluded-xexpr-tags '()]
                         #:xexpr-tag-proc [xexpr-tag-proc (λ(x)x)]
                         #:xexpr-attr-proc [xexpr-attr-proc (λ(x)x)]
                         #:xexpr-elements-proc [xexpr-elements-proc (λ(x)x)]
                         #:block-xexpr-proc [block-xexpr-proc (λ(x)x)]
                         #:inline-xexpr-proc [inline-xexpr-proc (λ(x)x)]
                         #:string-proc [string-proc (λ(x)x)])
  ;; use xexpr/c for contract because it gives better error messages
  ((xexpr/c) (#:exclude-xexpr-tags list?
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
