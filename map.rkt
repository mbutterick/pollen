#lang racket/base
(require xml xml/path racket/list racket/string racket/contract racket/match racket/set)
(require "tools.rkt" "world.rkt" "decode.rkt")

(require "tests/test.pmap")
;(require "tests/pollen-lang-test.p")

(module+ test (require rackunit))

(module+ test
  main
;  (define tt (main->tree (dynamic-require "tests/test.pmap" POLLEN_ROOT))))
 ) 

; get the values out of the file, or make them up
(define map-file (build-path START_DIR DEFAULT_MAP))
(define map-main empty)


;; todo: this ain't a function
(if (file-exists? map-file)
    ; load it, or ...
    (set! map-main (dynamic-require map-file POLLEN_ROOT)) 
    ; ... synthesize it
    (let ([files (directory-list START_DIR)])
      (set! files (map remove-ext (filter (λ(x) (has-ext? x POLLEN_SOURCE_EXT)) files)))
      (set! map-main (make-tagged-xexpr 'map-main empty (map path->string files)))))

;; todo: restrict this test 
;; all names must be unique
(define/contract (map-tree? x)
  (any/c . -> . boolean?)
  (and (tagged-xexpr? x) 
       (let ([locations (map ->string (flatten (filter-not-tree whitespace? (remove-attrs x))))])
         (= (len (apply set locations)) (len locations)))))

;; recursively processes tree, converting map locations & their parents into xexprs of this shape:
;; '(location ((parent "parent")))
(define/contract (add-parents x [parent empty])
  ((map-tree?) (xexpr-tag?) . ->* . map-tree?)
  ; disallow map-main as parent tag
  ;  (when (equal? parent 'map-main) (set! parent empty)) 
  (match x
    ;; this pattern signifies next level in hierarchy 
    ;; where first element is new parent, and rest are children.
    [(list (? xexpr-tag? next-parent) children ...)
     (let-values ([(tag attr _) (break-tagged-xexpr (add-parents next-parent parent))])
       ;; xexpr with tag as name, parent as attr, children as elements with tag as next parent
       (make-tagged-xexpr tag attr (map (λ(c) (add-parents c tag)) children)))]
    ;; single map entry: convert to xexpr with parent
    [else (make-tagged-xexpr (->symbol x) (make-xexpr-attr 'parent (->string parent)))]))

(module+ test
  (define test-map `(map-main "foo" "bar" ,(map-topic "one" (map-topic "two" "three"))))
  (check-equal? (add-parents test-map) 
                '(map-main ((parent "")) (foo ((parent "map-main"))) (bar ((parent "map-main"))) (one ((parent "map-main")) (two ((parent "one")) (three ((parent "two"))))))))

;; remove parents from tree (i.e., just remove attrs)
;; is not the inverse of add-parents, i.e., you do not get back your original input.
(define/contract (remove-parents mt) 
  (map-tree? . -> . map-tree?)
  (remove-attrs mt))

(module+ test
  (check-equal? (remove-parents 
                 '(map-main ((parent "")) (foo ((parent ""))) (bar ((parent ""))) 
                            (one ((parent "")) (two ((parent "one")) (three ((parent "two")))))))
                '(map-main (foo) (bar) (one (two (three))))))

;; todo: what is this for?
(define (main->tree main)
  (add-parents main))

;; todo: what is this for? to have default input?
(define tree (main->tree map-main))


(define/contract (map-key? x)
  (any/c . -> . boolean?)
  ;; OK for map-key to be #f
  (or (symbol? x) (string? x) (eq? x #f)))

;; return the parent of a given name
(define/contract (parent element [tree tree])
  ((map-key?) (map-tree?) . ->* . (or/c string? boolean?)) 
  (and element (let ([result (se-path* `(,(->symbol element) #:parent) tree)])
                 (and result (->string result))))) ; se-path* returns #f if nothing found


(module+ test
  (define test-tree (main->tree test-map))
  (check-equal? (parent 'three test-tree) "two")
  (check-equal? (parent "three" test-tree) "two")
  (check-false (parent 'nonexistent-name test-tree)))



; get children of a particular element
(define/contract (children element [tree tree])
  ((map-key?) (map-tree?) . ->* . (or/c list? boolean?))  
  ;; se-path*/list returns '() if nothing found
  (and element  (let ([children (se-path*/list `(,(->symbol element)) tree)])
                  ; If there are sublists, just take first element
                  (and (not (empty? children)) (map (λ(i) (->string (if (list? i) (car i) i))) children)))))

(module+ test
  (check-equal? (children 'one test-tree) (list "two"))
  (check-equal? (children 'two test-tree) (list "three"))
  (check-false (children 'three test-tree))
  (check-false (children 'fooburger test-tree)))


;; find all siblings on current level: go up to parent and ask for children
(define/contract (siblings element [tree tree])
  ;; this never returns false: element is always a sibling of itself.
  ;; todo: how to use input value in contract? e.g., to check that element is part of output list
  ((map-key?) (map-tree?) . ->* . (or/c list? boolean?))  
  (children (parent element tree) tree))

(module+ test
  (check-equal? (siblings 'one test-tree) '("foo" "bar" "one"))
  (check-equal? (siblings 'foo test-tree) '("foo" "bar" "one"))
  (check-equal? (siblings 'two test-tree) '("two"))
  (check-false (siblings 'invalid-key test-tree)))

;; helper function
(define/contract (side-siblings side element [tree tree])
  ((symbol? map-key?) (map-tree?) . ->* . (or/c list? boolean?))
  (define result ((if (equal? side 'left) takef takef-right) 
                  (siblings element tree) 
                  (λ(i) (not (equal? (->string element) (->string i))))))
  (and (not (empty? result)) result))


(define/contract (map-split element elements)
  (map-key? (listof map-key?) . -> . (values (listof map-key?) (listof map-key?)))
  (define-values (left right) (splitf-at elements 
                                         (λ(e) (not (equal? (->string e) (->string element))))))
  (values left (cdr right)))

(module+ test
  (check-equal? (values->list (map-split 'bar (siblings 'bar test-tree))) (list '("foo") '("one"))))
  

;; siblings to the left of target element (i.e., precede in map order)
(define (left-siblings element [tree tree])
  (side-siblings 'left element tree))

(module+ test
  (check-equal? (left-siblings 'one test-tree) '("foo" "bar"))
  (check-false (left-siblings 'foo test-tree)))

;; siblings to the right of target element (i.e., follow in map order)
(define (right-siblings element [tree tree])
  (side-siblings 'right element tree))

(module+ test
  (check-false (right-siblings 'one test-tree))
  (check-equal? (right-siblings 'foo test-tree) '("bar" "one")))


;; get element immediately to the left in map
(define/contract (left-sibling element [tree tree])
  ((map-key?) (map-tree?) . ->* . (or/c string? boolean?))
  (define siblings (left-siblings element tree))
  (and siblings (last siblings)))

(module+ test
  (check-equal? (left-sibling 'bar test-tree) "foo")
  (check-false (left-sibling 'foo test-tree)))

;; get element immediately to the right in map
(define/contract (right-sibling element [tree tree])
  ((map-key?) (map-tree?) . ->* . (or/c string? boolean?))
  (define siblings (right-siblings element tree))
  (and siblings (first siblings)))

(module+ test
  (check-equal? (right-sibling 'foo test-tree) "bar")
  (check-false (right-sibling 'one test-tree)))


;; flatten tree to sequence
(define/contract (make-page-sequence [tree tree])
  (map-tree? . -> . (listof string?))
  ; use cdr to get rid of main-map tag at front
  (map ->string (cdr (flatten (remove-parents tree))))) 

(module+ test
  (check-equal? (make-page-sequence test-tree) '("foo" "bar" "one" "two" "three")))

;; helper function for get-previous-pages and get-next-pages
(define/contract (adjacent-pages side element [tree tree])
  ((map-key? symbol?) (map-tree?) . ->* . (or/c list? boolean?))
  (define result ((if (equal? side 'left) takef takef-right)
                  (make-page-sequence tree) (λ(y) (not (equal? (->string element) (->string y))))))
  (and (not (empty? result)) result))

(module+ test
  (check-equal? (adjacent-pages 'left 'one test-tree) '("foo" "bar"))
  (check-equal? (adjacent-pages 'left 'three test-tree) '("foo" "bar" "one" "two"))
  (check-false (adjacent-pages 'left 'foo test-tree)))


;; get sequence of earlier pages
(define/contract (previous-pages element [tree tree])
  ((map-key?) (map-tree?) . ->* . (or/c list? boolean?))
  (adjacent-pages 'left element tree))

(module+ test
  (check-equal? (previous-pages 'one test-tree) '("foo" "bar"))
  (check-equal? (previous-pages 'three test-tree) '("foo" "bar" "one" "two"))
  (check-false (previous-pages 'foo test-tree)))


;; get sequence of next pages
(define (next-pages element [tree tree])
   ((map-key?) (map-tree?) . ->* . (or/c list? boolean?))
  (adjacent-pages 'right element tree))

(module+ test
  (check-equal? (next-pages 'foo test-tree) '("bar" "one" "two" "three"))
  (check-equal? (next-pages 'one test-tree) '("two" "three"))
  (check-false (next-pages 'three test-tree)))

;; get page immediately previous
(define/contract (previous-page element [tree tree])
  ((map-key?) (map-tree?) . ->* . (or/c string? boolean?))
  (define result (previous-pages element tree))
  (and result (last result)))

(module+ test
  (check-equal? (previous-page 'one test-tree) "bar")
  (check-equal? (previous-page 'three test-tree) "two")
  (check-false (previous-page 'foo test-tree)))

;; get page immediately next
(define (next-page element [tree tree])
  ((map-key?) (map-tree?) . ->* . (or/c string? boolean?))
  (define result (next-pages element tree))
  (and result (first result)))

(module+ test
  (check-equal? (next-page 'foo test-tree) "bar")
  (check-equal? (next-page 'one test-tree) "two")
  (check-false (next-page 'three test-tree)))

(provide (all-defined-out))