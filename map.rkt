#lang racket/base
(require xml xml/path racket/list racket/string racket/contract racket/match)
;; todo: why is this require here?
(require (except-in web-server/templates in))
(require "tools.rkt" "world.rkt")

(module+ test (require rackunit))

(module+ test
  (define tt (main->tree (dynamic-require "tests/test.pmap" POLLEN_ROOT))))

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
(define/contract (pmap-tree? x)
  (any/c . -> . boolean?)
  (tagged-xexpr? x))

;; recursively processes tree, converting map locations & their parents into xexprs of this shape:
;; '(location ((parent "parent")))
(define/contract (add-parents x [parent empty])
  ((pmap-tree?) (xexpr-tag?) . ->* . pmap-tree?)
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
(define/contract (remove-parents x) 
  (pmap-tree? . -> . tagged-xexpr?)
  (match x
    [(? tagged-xexpr?) (let-values ([(tag attr elements) (break-tagged-xexpr x)])
                         (make-tagged-xexpr tag empty (remove-parents elements)))]
    [(? list?) (map remove-parents x)]
    [else x]))

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
(define/contract (get-parent element [tree tree])
  ((map-key?) (pmap-tree?) . ->* . (or/c string? boolean?)) 
  (and element (let ([result (se-path* `(,(->symbol element) #:parent) tree)])
                 (and result (->string result))))) ; se-path* returns #f if nothing found


(module+ test
  (define test-tree (main->tree test-map))
  (check-equal? (get-parent 'three test-tree) "two")
  (check-equal? (get-parent "three" test-tree) "two")
  (check-false (get-parent 'nonexistent-name test-tree)))



; get children of a particular element
(define/contract (get-children element [tree tree])
  ((map-key?) (pmap-tree?) . ->* . (or/c list? boolean?))  
  ;; se-path*/list returns '() if nothing found
  (and element  (let ([children (se-path*/list `(,(->symbol element)) tree)])
                  ; If there are sublists, just take first element
                  (and (not (empty? children)) (map (λ(i) (->string (if (list? i) (car i) i))) children)))))

(module+ test
  (check-equal? (get-children 'one test-tree) (list "two"))
  (check-equal? (get-children 'two test-tree) (list "three"))
  (check-false (get-children 'three test-tree))
  (check-false (get-children 'fooburger test-tree)))


;; find all siblings on current level: go up to parent and ask for children
(define/contract (get-all-siblings element [tree tree])
  ;; this never returns false: element is always a sibling of itself.
  ;; todo: how to use input value in contract? e.g., to check that element is part of output list
  ((map-key?) (pmap-tree?) . ->* . (or/c list? boolean?))  
  (get-children (get-parent element tree) tree))

(module+ test
  (check-equal? (get-all-siblings 'one test-tree) '("foo" "bar" "one"))
  (check-equal? (get-all-siblings 'foo test-tree) '("foo" "bar" "one"))
  (check-equal? (get-all-siblings 'two test-tree) '("two"))
  (check-false (get-all-siblings 'invalid-key test-tree)))

;; helper function
(define/contract (get-side-siblings side element [tree tree])
  ((symbol? map-key?) (pmap-tree?) . ->* . (or/c list? boolean?))
  (define result ((if (equal? side 'left) takef takef-right) 
                  (get-all-siblings element tree) 
                  (λ(i) (not (equal? (->string element) (->string i))))))
  (and (not (empty? result)) result))


;; siblings to the left of target element (i.e., precede in map order)
(define (get-left-siblings element [tree tree])
  (get-side-siblings 'left element tree))

(module+ test
  (check-equal? (get-left-siblings 'one test-tree) '("foo" "bar"))
  (check-false (get-left-siblings 'foo test-tree)))

;; siblings to the right of target element (i.e., follow in map order)
(define (get-right-siblings element [tree tree])
  (get-side-siblings 'right element tree))

(module+ test
  (check-false (get-right-siblings 'one test-tree))
  (check-equal? (get-right-siblings 'foo test-tree) '("bar" "one")))


;; get element immediately to the left in map
(define/contract (get-left element [tree tree])
  ((map-key?) (pmap-tree?) . ->* . (or/c string? boolean?))
  (define siblings (get-left-siblings element tree))
  (and siblings (last siblings)))

(module+ test
  (check-equal? (get-left 'bar test-tree) "foo")
  (check-false (get-left 'foo test-tree)))

;; get element immediately to the right in map
(define/contract (get-right element [tree tree])
  ((map-key?) (pmap-tree?) . ->* . (or/c string? boolean?))
  (define siblings (get-right-siblings element tree))
  (and siblings (first siblings)))

(module+ test
  (check-equal? (get-right 'foo test-tree) "bar")
  (check-false (get-right 'one test-tree)))


;; flatten tree to sequence
(define/contract (make-page-sequence [tree tree])
  (pmap-tree? . -> . (listof string?))
  ; use cdr to get rid of main-map tag at front
  (map ->string (cdr (flatten (remove-parents tree))))) 

(module+ test
  (check-equal? (make-page-sequence test-tree) '("foo" "bar" "one" "two" "three")))

;; helper function for get-previous-pages and get-next-pages
(define/contract (get-adjacent-pages side element [tree tree])
  ((map-key? symbol?) (pmap-tree?) . ->* . (or/c list? boolean?))
  (define result ((if (equal? side 'left) takef takef-right)
                  (make-page-sequence tree) (λ(y) (not (equal? (->string element) (->string y))))))
  (and (not (empty? result)) result))

(module+ test
  (check-equal? (get-adjacent-pages 'left 'one test-tree) '("foo" "bar"))
  (check-equal? (get-adjacent-pages 'left 'three test-tree) '("foo" "bar" "one" "two"))
  (check-false (get-adjacent-pages 'left 'foo test-tree)))


;; get sequence of earlier pages
(define/contract (get-previous-pages element [tree tree])
  ((map-key?) (pmap-tree?) . ->* . (or/c list? boolean?))
  (get-adjacent-pages 'left element tree))

(module+ test
  (check-equal? (get-previous-pages 'one test-tree) '("foo" "bar"))
  (check-equal? (get-previous-pages 'three test-tree) '("foo" "bar" "one" "two"))
  (check-false (get-previous-pages 'foo test-tree)))


;; get sequence of next pages
(define (get-next-pages element [tree tree])
   ((map-key?) (pmap-tree?) . ->* . (or/c list? boolean?))
  (get-adjacent-pages 'right element tree))

(module+ test
  (check-equal? (get-next-pages 'foo test-tree) '("bar" "one" "two" "three"))
  (check-equal? (get-next-pages 'one test-tree) '("two" "three"))
  (check-false (get-next-pages 'three test-tree)))

;; get page immediately previous
(define/contract (get-previous element [tree tree])
  ((map-key?) (pmap-tree?) . ->* . (or/c string? boolean?))
  (define result (get-previous-pages element tree))
  (and result (last result)))

(module+ test
  (check-equal? (get-previous 'one test-tree) "bar")
  (check-equal? (get-previous 'three test-tree) "two")
  (check-false (get-previous 'foo test-tree)))

;; get page immediately next
(define (get-next element [tree tree])
  ((map-key?) (pmap-tree?) . ->* . (or/c string? boolean?))
  (define result (get-next-pages element tree))
  (and result (first result)))

(module+ test
  (check-equal? (get-next 'foo test-tree) "bar")
  (check-equal? (get-next 'one test-tree) "two")
  (check-false (get-next 'three test-tree)))

;; todo: why is this re-exporting web-server/templates?
(provide (all-defined-out) (all-from-out web-server/templates))