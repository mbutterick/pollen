#lang racket/base
(require racket/contract racket/match racket/list xml)
(require (prefix-in scribble: (only-in scribble/decode whitespace?)))
(require (prefix-in html: "library/html.rkt"))
(require "world.rkt" "readability.rkt" "pollen-file-tools.rkt")

(module+ test (require rackunit))

(provide (all-defined-out)
         (all-from-out "pollen-file-tools.rkt"))


;; add a block tag to the list
;; this function is among the predicates because it alters a predicate globally.
(define/contract (register-block-tag tag)
  (symbol? . -> . void?)
  (set! block-tags (cons tag block-tags)))

;; initial set of block tags: from html
(define block-tags html:block-tags)


;; is the tagged-xexpr a block element (as opposed to inline)
;; tags are inline unless they're registered as block tags.
(define/contract (block-xexpr? x)
  (any/c . -> . boolean?)
  ((tagged-xexpr? x) . and . (->boolean ((car x) . in? . block-tags))))

(module+ test
  (check-true (block-xexpr? '(p "foo")))
  (check-true (block-xexpr? '(div "foo")))
  (check-false (block-xexpr? '(em "foo")))
  (check-false (block-xexpr? '(barfoo "foo")))
  (check-true (begin (register-block-tag 'barfoo) (block-xexpr? '(barfoo "foo")))))


;; is it an xexpr tag?
(define/contract (xexpr-tag? x)
  (any/c . -> . boolean?)
  (symbol? x)) 

;; is it an xexpr attributes?
(define/contract (xexpr-attr? x)
  (any/c . -> . boolean?)
  (match x
    ; list of symbol + string pairs
    [(list (list (? symbol? key) (? string? value)) ...) #t]
    [else #f]))

(module+ test  
  (check-true (xexpr-attr? empty))
  (check-true (xexpr-attr? '((key "value"))))
  (check-true (xexpr-attr? '((key "value") (foo "bar"))))
  (check-false (xexpr-attr? '((key "value") "foo" "bar"))) ; content, not attr
  (check-false (xexpr-attr? '(key "value"))) ; not a nested list
  (check-false (xexpr-attr? '(("key" "value")))) ; two strings
  (check-false (xexpr-attr? '((key value))))) ; two symbols


;; is it xexpr content?
(define/contract (xexpr-element? x)
  (any/c . -> . boolean?)
  (or (string? x) (tagged-xexpr? x)))


(define/contract (xexpr-elements? x)
  (any/c . -> . boolean?)
  (match x
    ;; this is more strict than xexpr definition in xml module
    ;; don't allow symbols or numbers to be part of content
    [(list elem ...) (andmap xexpr-element? elem)]
    [else #f]))

(module+ test  
  (check-true (xexpr-elements? '("p" "foo" "123")))
  (check-false (xexpr-elements? "foo")) ; not a list
  (check-false (xexpr-elements? '("p" "foo" 123))) ; includes number
  (check-false (xexpr-elements? '(p "foo" "123"))) ; includes symbol
  (check-false (xexpr-elements? '(((key "value")) "foo" "bar"))) ; includes attr
  (check-false (xexpr-elements? '("foo" "bar" ((key "value")))))) ; malformed


;; is it a named x-expression?
;; todo: rewrite this recurively so errors can be pinpointed (for debugging)
(define/contract (tagged-xexpr? x)
  (any/c . -> . boolean?)
  (and (xexpr? x) ; meets basic xexpr contract
       (match x
         [(list (? symbol? name) rest ...) ; is a list starting with a symbol
          (or (xexpr-elements? rest) ; the rest is content or ...
              (and (xexpr-attr? (car rest)) (xexpr-elements? (cdr rest))))] ; attr + content 
         [else #f])))

(module+ test  
  (check-true (tagged-xexpr? '(p "foo" "bar")))
  (check-true (tagged-xexpr? '(p ((key "value")) "foo" "bar")))
  (check-false (tagged-xexpr? "foo")) ; not a list with symbol
  (check-false (tagged-xexpr? '(p "foo" "bar" ((key "value"))))) ; malformed
  (check-false (tagged-xexpr? '("p" "foo" "bar"))) ; no name
  (check-false (tagged-xexpr? '(p 123)))) ; content is a number


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


;; recursive whitespace test
;; Scribble's version misses whitespace in a list
(define (whitespace? x)
  (cond
    [(list? x) (andmap whitespace? x)]
    [else (scribble:whitespace? x)]))

(module+ test
  (check-true (whitespace? " "))
  (check-false (whitespace? "foo"))
  (check-false (whitespace? " ")) ; a nonbreaking space
  (check-true (whitespace? "\n \n"))
  (check-true (whitespace? (list "\n" " " "\n")))
  (check-true (whitespace? (list "\n" " " "\n" (list "\n" "\n")))))


