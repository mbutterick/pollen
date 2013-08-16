#lang racket/base
(require racket/contract racket/match)
(require (only-in racket/path filename-extension))
(require (only-in racket/format ~a))
(require racket/list)
(require (only-in racket/string string-join))
(require (only-in xml xexpr? xexpr/c))
(require (prefix-in scribble: (only-in scribble/decode whitespace?)))


(require "readability.rkt" "debug.rkt")
(provide (all-defined-out) (all-from-out "readability.rkt" "debug.rkt"))

;; setup for test cases
(module+ test
  (require rackunit)
  (define foo-path-strings '("foo" "foo.txt" "foo.bar" "foo.bar.txt"))
  (define-values (foo-path foo.txt-path foo.bar-path foo.bar.txt-path) (apply values (map string->path foo-path-strings)))
  ;; test the sample paths before using them for other tests
  (define foo-paths (list foo-path foo.txt-path foo.bar-path foo.bar.txt-path))
  (for-each check-equal? (map path->string foo-paths) foo-path-strings))


;; recursive whitespace test
;; Scribble's version misses whitespace in a list
(define (whitespace? x)
  (cond
    [(list? x) (andmap whitespace? x)]
    [else (scribble:whitespace? x)]))

; make these independent of local includes
(define (map-topic topic . subtopics)
  (make-tagged-xexpr (->symbol topic) empty (filter-not whitespace? subtopics)))


;; does path have a certain extension
(define/contract (has-ext? path ext)
  (path? symbol? . -> . boolean?)
  (define ext-of-path (filename-extension path))
  (and ext-of-path (equal? (bytes->string/utf-8 ext-of-path) (->string ext))))

(module+ test
  (check-false (has-ext? foo-path 'txt)) 
  (check-true (has-ext? foo.txt-path 'txt))
  (check-true (has-ext? foo.bar.txt-path 'txt))
  (check-false (has-ext? foo.bar.txt-path 'doc))) ; wrong extension


;; put extension on path
(define/contract (add-ext path ext)
  (path? (or/c symbol? string?) . -> . path?)
  (string->path (string-append (->string path) "." (->string ext))))

(module+ test
  (check-equal? (add-ext (string->path "foo") "txt") (string->path "foo.txt")))

;; take one extension off path
(define/contract (remove-ext path)
  (path? . -> . path?)
  (path-replace-suffix path ""))

(module+ test  
  (check-equal? (remove-ext foo-path) foo-path)
  (check-equal? (remove-ext foo.txt-path) foo-path)
  (check-equal? (remove-ext foo.bar.txt-path) foo.bar-path)
  (check-not-equal? (remove-ext foo.bar.txt-path) foo-path)) ; does not remove all extensions


;; take all extensions off path
(define/contract (remove-all-ext path)
  (path? . -> . path?)
  (define path-with-removed-ext (remove-ext path))
  (if (equal? path path-with-removed-ext)
      path
      (remove-all-ext path-with-removed-ext)))

(module+ test  
  (check-equal? (remove-all-ext foo-path) foo-path)
  (check-equal? (remove-all-ext foo.txt-path) foo-path)
  (check-not-equal? (remove-all-ext foo.bar.txt-path) foo.bar-path) ; removes more than one ext
  (check-equal? (remove-all-ext foo.bar.txt-path) foo-path))

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



;; helper for comparison of values
;; normal function won't work for this. Has to be syntax-rule
(define-syntax-rule (values->list vs)
  (call-with-values (λ() vs) list))


;; convert list of alternating keys & values to attr
;; todo: make contract. Which is somewhat complicated:
;; list of items, made of xexpr-attr or even numbers of symbol/string pairs
;; use splitf*-at with xexpr-attr? as test, then check lengths of resulting lists
(define/contract (make-xexpr-attr . items)
  (() #:rest (listof (λ(i) (or (xexpr-attr? i) (symbol? i) (string? i)))) . ->* . xexpr-attr?)
  
  ;; need this function to make sure that 'foo and "foo" are treated as the same hash key
  (define (make-attr-list items)
    (if (empty? items)
        empty
        (let ([key (->symbol (first items))]
              [value (->string (second items))]
              [rest (drop items 2)])
          (append (list key value) (make-attr-list rest)))))
  
  ;; use flatten to splice xexpr-attrs into list
  ;; use hash to ensure keys are unique (later values will overwrite earlier)
  (define attr-hash (apply hash (make-attr-list (flatten items))))
  `(,@(map (λ(k) (list k (get attr-hash k))) 
           ;; sort needed for predictable results for unit tests
           (sort (hash-keys attr-hash) (λ(a b) (string<? (->string a) (->string b)))))))

(module+ test
  (check-equal? (make-xexpr-attr 'foo "bar") '((foo "bar")))
  (check-equal? (make-xexpr-attr "foo" 'bar) '((foo "bar")))
  (check-equal? (make-xexpr-attr "foo" "bar" "goo" "gar") '((foo "bar")(goo "gar")))
  (check-equal? (make-xexpr-attr (make-xexpr-attr "foo" "bar" "goo" "gar") "hee" "haw") 
                '((foo "bar")(goo "gar")(hee "haw")))
  (check-equal? (make-xexpr-attr '((foo "bar")(goo "gar")) "foo" "haw") '((foo "haw")(goo "gar"))))


;; create tagged-xexpr from parts (opposite of break-tagged-xexpr)
(define/contract (make-tagged-xexpr name [attr empty] [content empty])
  ((symbol?) (xexpr-attr? xexpr-elements?) . ->* . tagged-xexpr?)
  (filter-not empty? `(,name ,attr ,@content)))

(module+ test
  (check-equal? (make-tagged-xexpr 'p) '(p))
  (check-equal? (make-tagged-xexpr 'p '((key "value"))) '(p ((key "value"))))
  (check-equal? (make-tagged-xexpr 'p empty '("foo" "bar")) '(p "foo" "bar"))
  (check-equal? (make-tagged-xexpr 'p '((key "value")) (list "foo" "bar")) 
                '(p ((key "value")) "foo" "bar")))


;; decompose tagged-xexpr into parts (opposite of make-tagged-xexpr)
(define/contract (break-tagged-xexpr nx)
  (tagged-xexpr? . -> . (values symbol? xexpr-attr? xexpr-elements?))
  (match 
      ; tagged-xexpr may or may not have attr
      ; if not, add empty attr so that decomposition only handles one case
      (match nx
        [(list _ (? xexpr-attr?) _ ...) nx]
        [else `(,(car nx) ,empty ,@(cdr nx))])
    [(list tag attr content ...) (values tag attr content)]))

(module+ test
  (check-equal? (values->list (break-tagged-xexpr '(p))) 
                (values->list (values 'p empty empty)))
  (check-equal? (values->list (break-tagged-xexpr '(p "foo"))) 
                (values->list (values 'p empty '("foo"))))
  (check-equal? (values->list (break-tagged-xexpr '(p ((key "value"))))) 
                (values->list (values 'p '((key "value")) empty)))
  (check-equal? (values->list (break-tagged-xexpr '(p ((key "value")) "foo"))) 
                (values->list (values 'p '((key "value")) '("foo")))))


;; convenience functions to retrieve only one part of tagged-xexpr
(define (tagged-xexpr-tag nx)
  (tagged-xexpr? . -> . xexpr-tag?)
  (define-values (tag attr content) (break-tagged-xexpr nx))
  tag)

(define (tagged-xexpr-attr nx)
  (tagged-xexpr? . -> . xexpr-attr?)
  (define-values (tag attr content) (break-tagged-xexpr nx))
  attr)

(define (tagged-xexpr-elements nx)
  (tagged-xexpr? . -> . xexpr-elements?)
  (define-values (tag attrt elements) (break-tagged-xexpr nx))
  elements)

(module+ test
  (check-equal? (tagged-xexpr-tag '(p ((key "value"))"foo" "bar" (em "square"))) 'p)
  (check-equal? (tagged-xexpr-attr '(p ((key "value"))"foo" "bar" (em "square"))) '((key "value")))
  (check-equal? (tagged-xexpr-elements '(p ((key "value"))"foo" "bar" (em "square"))) 
                '("foo" "bar" (em "square"))))


;; remove all attr blocks (helper function)
(define/contract (remove-attrs x)
  (tagged-xexpr? . -> . tagged-xexpr?)
  (match x
    [(? tagged-xexpr?) (let-values ([(tag attr elements) (break-tagged-xexpr x)])
                         (make-tagged-xexpr tag empty (remove-attrs elements)))]
    [(? list?) (map remove-attrs x)]
    [else x]))

(module+ test
  (check-equal? (remove-attrs '(p ((foo "bar")) "hi")) '(p "hi"))
  (check-equal? (remove-attrs '(p ((foo "bar")) "hi" (p ((foo "bar")) "hi"))) '(p "hi" (p "hi"))))
  

;; apply filter proc recursively
(define/contract (filter-tree proc tree)
  (procedure? list? . -> . list?)
  (define (remove-empty x)
    (cond
      [(list? x) (filter-not empty? (map remove-empty x))]
      [else x]))
  
  (define (filter-tree-inner proc x)
    (cond
      [(list? x) (map (λ(i) (filter-tree-inner proc i)) x)]
      [else (if (proc x) x empty)]))
  
  (remove-empty (filter-tree-inner proc tree)))


(module+ test
  (check-equal? (filter-tree string? '(p)) empty)
  (check-equal? (filter-tree string? '(p "foo" "bar")) '("foo" "bar"))
  (check-equal? (filter-tree string? '(p "foo" (p "bar"))) '("foo" ("bar")))
  (check-equal? (filter-tree (λ(i) (and (string? i) (equal? i "\n"))) '("\n" (foo "bar") "\n")) '("\n" "\n"))) 

;; apply filter-not proc recursively
(define/contract (filter-not-tree proc tree)
  (procedure? list? . -> . list?)
  (filter-tree (λ(i) (not (proc i))) tree))

(module+ test
  (check-equal? (filter-not-tree string? '(p)) '(p))
  (check-equal? (filter-not-tree string? '(p "foo" "bar")) '(p))
  (check-equal? (filter-not-tree string? '(p "foo" (p "bar"))) '(p (p)))
  ;(check-equal? (filter-tree (λ(i) (and (tagged-xexpr? i) (equal? 'em (car i)))) '(p "foo" (em "bar"))) '(p "foo"))
  )


;; todo: doc this function
(define/contract (map-tree proc tree)
  (procedure? list? . -> . list?)
  (cond 
    [(list? tree) (map (λ(i) (map-tree proc i)) tree)]
    [else (proc tree)]))

(module+ test
  (check-equal? (map-tree (λ(i) (if (number? i) (* 2 i) i)) '(p 1 2 3 (em 4 5))) '(p 2 4 6 (em 8 10)))
  (check-equal? (map-tree (λ(i) (if (symbol? i) 'foo i)) '(p 1 2 3 (em 4 5))) '(foo 1 2 3 (foo 4 5))))
