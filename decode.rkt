#lang racket/base
(require racket/contract racket/list racket/string racket/match)
(require (only-in racket/format ~a))
(require (only-in racket/bool nor))
(require (only-in xml xexpr/c))
(require (prefix-in scribble: (only-in scribble/decode whitespace?)))
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
  (->boolean ((named-xexpr-name nx) . in . block-tags)))

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



; trim from beginning & end of list
(define (trim items test-proc)
  (dropf-right (dropf items test-proc) test-proc))

(module+ test
  (check-equal? (trim (list "\n" " " 1 2 3 "\n") whitespace?) '(1 2 3))
  (check-equal? (trim (list 1 3 2 4 5 6 8 9 13) odd?) '(2 4 5 6 8)))

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


#|

(define (make-meta-hash x)
  (define keys (se-path*/list '(meta #:name) x))
  (define values (se-path*/list '(meta #:content) x))
  (define meta-hash (make-hash))
  ;todo: convert this to for/list because map does not guarantee ordering
  ; probably want to keep it in sequence
  (map (ƒ(key value) (change meta-hash (as-symbol key) (as-string value))) keys values)
  meta-hash)

|#


;; decoder wireframe
(define/contract (decode nx
                         #:exclude-xexpr-names [excluded-xexpr-names '()]
                         #:xexpr-name-proc [xexpr-name-proc (λ(x)x)]
                         #:xexpr-attr-proc [xexpr-attr-proc (λ(x)x)]
                         #:xexpr-content-proc [xexpr-content-proc #f] ; set this to &decode later
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
  
  (define metas (list))
  (define/contract (is-meta? x)
    (any/c . -> . (λ(i) (or (boolean? i) (list? i))))
    (match x
      [`(meta ,(? string? key) ,(? string? value)) (list key value)]
      [else #f]))
  
  (define (&decode x)
    (cond
      [(named-xexpr? x) (let-values([(name attr content) (break-named-xexpr x)]) 
                          (if (name . in . (->list excluded-xexpr-names))    
                              x
                              (let ([decoded-xexpr 
                                     (apply make-named-xexpr (map &decode (list name attr content)))])
                                (if (block-xexpr? decoded-xexpr)
                                    (block-xexpr-proc decoded-xexpr)
                                    (inline-xexpr-proc decoded-xexpr)))))]
      [(xexpr-name? x) (xexpr-name-proc x)]
      [(xexpr-attr? x) (xexpr-attr-proc x)]
      [(xexpr-content? x) (let ([xexpr-content-proc (or xexpr-content-proc (λ(x) (map &decode x)))])
                            (xexpr-content-proc x))]
      [(string? x) (string-proc x)]
      [else x]))
  
  ;; function to strip metas
  ;; todo: would this be simpler using se-path*/list?
  (define (split-metas nx)
    (define meta-list '())
    (define (&split-metas x)
      (cond
        [(and (named-xexpr? x) (equal? 'meta (car x)))
         (begin
           (set! meta-list (cons x meta-list))
           empty)]
        [(named-xexpr? x) ; handle named-xexpr
         (let-values([(name attr body) (break-named-xexpr x)]) 
           (make-named-xexpr name attr (&split-metas body)))]
        [(list? x) (filter-not empty? (map &split-metas x))]
        [else x]))
    (values (&split-metas nx) (reverse meta-list))) 
  
  ;; put metas back on the end
  (define (append-metas nx metas)
    (named-xexpr? . -> . named-xexpr?)
    (append nx (map meta-proc metas)))
  
  (let-values ([(nx metas) (split-metas nx)])
    (append-metas (&decode nx) metas)))

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