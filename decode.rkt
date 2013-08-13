#lang racket/base
(require racket/contract racket/list racket/string racket/match)
(require (only-in racket/format ~a))
(require (only-in racket/bool nor))
(require (only-in xml xexpr/c))
(module+ test (require rackunit))

(require "tools.rkt")
(require (prefix-in html: "library/html.rkt"))
(provide (all-defined-out))


;; split list into list of sublists using test-proc
(define/contract (splitf-at* xs test-proc)
  (list? procedure? . -> . (λ(i) (match i [(list (? list?) ...) #t][else #f])))
  (define (&splitf-at* pieces [acc '()]) ; use acc for tail recursion
    (if (empty? pieces) 
        acc
        (let-values ([(item rest) 
                      (splitf-at (dropf pieces test-proc) (compose1 not test-proc))])
          (&splitf-at* rest `(,@acc ,item)))))
  (&splitf-at* (trim xs test-proc)))

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


(define block-tags html:block-tags)
(define (register-block-tag tag)
  (set! block-tags (cons tag block-tags)))

;; todo: add native support for list-xexpr
;; decode triple newlines to list items

;; is the tagged-xexpr a block element (as opposed to inline)
;; tags are inline unless they're registered as block tags.
(define/contract (block-xexpr? x)
  (any/c . -> . boolean?)
  ((tagged-xexpr? x) . and . (->boolean ((tagged-xexpr-tag x) . in . block-tags))))

(module+ test
  (check-true (block-xexpr? '(p "foo")))
  (check-true (block-xexpr? '(div "foo")))
  (check-false (block-xexpr? '(em "foo")))
  (check-false (block-xexpr? '(barfoo "foo")))
  (check-true (begin (register-block-tag 'barfoo) (block-xexpr? '(barfoo "foo")))))


;; convert numbers to strings
;; maybe this isn't necessary
(define (stringify x)
  (map-tree (λ(i) (if (number? i) (->string i) i)) x))

(module+ test
  (check-equal? (stringify '(p 1 2 "foo" (em 4 "bar"))) '(p "1" "2" "foo" (em "4" "bar"))))





(module+ test
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
  (any/c . -> . boolean?)
  (match x
    [`(meta ,(? string? key) ,(? string? value)) #t]
    [else #f]))

(module+ test
  (check-true (meta-xexpr? '(meta "key" "value")))
  (check-false (meta-xexpr? '(meta "key" "value" "foo")))
  (check-false (meta-xexpr? '(meta))))


;; function to strip metas (or any tag)
(define/contract (extract-tag-from-xexpr tag nx)
  (xexpr-tag? tagged-xexpr? . -> . (values tagged-xexpr? xexpr-elements?))
  (define matches '())
  (define (extract-tag x)
    (cond
      [(and (tagged-xexpr? x) (equal? tag (car x)))
       ; stash matched tag but return empty value
       (begin
         (set! matches (cons x matches))
         empty)]
      [(tagged-xexpr? x) (let-values([(tag attr body) (break-tagged-xexpr x)]) 
                           (make-tagged-xexpr tag attr (extract-tag body)))]
      [(xexpr-elements? x) (filter-not empty? (map extract-tag x))]
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
                         #:exclude-xexpr-tags [excluded-xexpr-tags '()]
                         #:xexpr-tag-proc [xexpr-tag-proc (λ(x)x)]
                         #:xexpr-attr-proc [xexpr-attr-proc (λ(x)x)]
                         #:xexpr-elements-proc [xexpr-elements-proc (λ(x)x)]
                         #:block-xexpr-proc [block-xexpr-proc (λ(x)x)]
                         #:inline-xexpr-proc [inline-xexpr-proc (λ(x)x)]
                         #:string-proc [string-proc (λ(x)x)]
                         #:meta-proc [meta-proc (λ(x)x)])
  ;; use xexpr/c for contract because it gives better error messages
  ((xexpr/c) (#:exclude-xexpr-tags (λ(i) (or (symbol? i) (list? i)))
                                   #:xexpr-tag-proc procedure?
                                   #:xexpr-attr-proc procedure?
                                   #:xexpr-elements-proc procedure?
                                   #:block-xexpr-proc procedure?
                                   #:inline-xexpr-proc procedure?
                                   #:string-proc procedure?
                                   #:meta-proc procedure?)
             . ->* . tagged-xexpr?)
  (when (not (tagged-xexpr? nx))
    (error (format "decode: ~v not a full tagged-xexpr" nx)))
  
  (define metas (list))
  
  (define (&decode x)
    (cond
      [(tagged-xexpr? x) (let-values([(tag attr elements) (break-tagged-xexpr x)]) 
                           (if (tag . in . (->list excluded-xexpr-tags))    
                               x
                               (let ([decoded-xexpr 
                                      (apply make-tagged-xexpr (map &decode (list tag attr elements)))])
                                 ((if (block-xexpr? decoded-xexpr)
                                      block-xexpr-proc
                                      inline-xexpr-proc) decoded-xexpr))))]
      [(xexpr-tag? x) (xexpr-tag-proc x)]
      [(xexpr-attr? x) (xexpr-attr-proc x)]
      [(xexpr-elements? x) (map &decode (xexpr-elements-proc x))]
      [(string? x) (string-proc x)]
      [else x]))
  
  
  (let-values ([(nx metas) (extract-tag-from-xexpr 'meta nx)])
    (append (&decode nx) (map meta-proc metas))))

