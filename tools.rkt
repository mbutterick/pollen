#lang racket/base
(require racket/contract racket/match racket/path)
(require (only-in racket/format ~a))
(require racket/list)
(require (only-in racket/string string-join))
(require (only-in xml xexpr? xexpr/c))

(require sugar "debug.rkt" "predicates.rkt" "world.rkt")
(provide (all-defined-out) (all-from-out sugar "debug.rkt" "predicates.rkt" racket/list racket/path))

;; setup for test cases
(module+ test (require rackunit))

;; list of all eligible requires in project require directory
(define/contract (get-project-require-files)
  (-> (or/c (listof complete-path?) boolean?))
  (define extras-directory (build-path PROJECT_ROOT EXTRAS_DIR))
  (and (directory-exists? extras-directory)
       ;; #:build? option returns complete paths (instead of just file names)
       (let ([files (filter project-require-file? (directory-list extras-directory #:build? #t))])
         (and (not (empty? files)) files))))


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
  ; xexpr/c provides a nicer error message,
  ; but is not sufficient on its own (too permissive)
  ((symbol?) (xexpr-attr? (listof xexpr-element?)) 
             . ->* . tagged-xexpr?)
  (filter-not empty? `(,name ,attr ,@content)))

(module+ test
  (check-equal? (make-tagged-xexpr 'p) '(p))
  (check-equal? (make-tagged-xexpr 'p '((key "value"))) '(p ((key "value"))))
  (check-equal? (make-tagged-xexpr 'p empty '("foo" "bar")) '(p "foo" "bar"))
  (check-equal? (make-tagged-xexpr 'p '((key "value")) (list "foo" "bar")) 
                '(p ((key "value")) "foo" "bar")))


;; decompose tagged-xexpr into parts (opposite of make-tagged-xexpr)
(define/contract (break-tagged-xexpr nx)
  (tagged-xexpr? . -> . 
                 (values symbol? xexpr-attr? (listof xexpr-element?)))
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
  (tagged-xexpr? . -> . (listof xexpr-element?))
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


(define/contract (map-xexpr-elements proc tx)
  (procedure? tagged-xexpr? . -> . tagged-xexpr?)
  (define-values (tag attr elements) (break-tagged-xexpr tx)) 
  (make-tagged-xexpr tag attr (map proc elements)))

(module+ test
  (check-equal? (map-xexpr-elements (λ(x) (if (string? x) "boing" x))  
                                    '(p "foo" "bar" (em "square"))) 
                '(p "boing" "boing" (em "square"))))




;; function to split tag out of tagged-xexpr
(define/contract (split-tag-from-xexpr tag tx)
  (xexpr-tag? tagged-xexpr? . -> . (values (listof xexpr-element?) tagged-xexpr? ))
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
  (define tx-extracted (extract-tag tx)) ;; do this first to fill matches
  (values (reverse matches) tx-extracted)) 


(module+ test
  (define xx '(root (meta "foo" "bar") "hello" "world" (meta "foo2" "bar2") 
                    (em "goodnight" "moon" (meta "foo3" "bar3"))))
  
  (check-equal? (values->list (split-tag-from-xexpr 'meta xx)) 
                (list '((meta "foo" "bar") (meta "foo2" "bar2") (meta "foo3" "bar3")) 
                      '(root "hello" "world" (em "goodnight" "moon")))))


;; convert list of meta tags to a hash for export from pollen document.
;; every meta is form (meta "key" "value") (enforced by contract)
;; later metas with the same name will override earlier ones.
(define/contract (make-meta-hash mxs)
  ((listof meta-xexpr?) . -> . hash?)
  (apply hash (append-map tagged-xexpr-elements mxs)))

(module+ test
  (check-equal? (make-meta-hash '((meta "foo" "bar")(meta "hee" "haw")))
                (hash "foo" "bar" "hee" "haw"))
  (check-equal? (make-meta-hash '((meta "foo" "bar")(meta "foo" "haw")))
                (hash "foo" "haw")))


