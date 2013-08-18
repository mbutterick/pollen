#lang racket/base
(require xml xml/path racket/list racket/string racket/contract racket/match racket/set)
(require "tools.rkt" "world.rkt" "decode.rkt")

(module+ test (require rackunit))

(provide (all-defined-out))

; get the values out of the file, or make them up
(define pmap-file (build-path START_DIR DEFAULT_MAP))
(define pmap-main empty)

;; todo: this ain't a function
(if (file-exists? pmap-file)
    ; load it, or ...
    (set! pmap-main (dynamic-require pmap-file POLLEN_ROOT)) 
    ; ... synthesize it
    (let ([files (directory-list START_DIR)])
      (set! files (map remove-ext (filter (λ(x) (has-ext? x POLLEN_SOURCE_EXT)) files)))
      (set! pmap-main (make-tagged-xexpr 'map-main empty (map path->string files)))))

;; todo: restrict this test 
;; all names must be unique
(define/contract (pmap? x)
  (any/c . -> . boolean?)
  (and (tagged-xexpr? x) 
       ;; all locations must be unique. Check this by converting x to a list of strings ...
       (let ([locations (map ->string (flatten (remove-attrs x)))])
         ;; and then coercing to set (because set impliedly enforces uniqueness)
         ;; If set has same number of elements as original, all are unique.
         (= (len (apply set locations)) (len locations)))))

;; recursively processes map, converting map locations & their parents into xexprs of this shape:
;; '(location ((parent "parent")))
(define/contract (add-parents x [parent empty])
  ((pmap?) (xexpr-tag?) . ->* . pmap?)
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
  (define test-pmap-main `(pmap-main "foo" "bar" ,(pmap-topic "one" (pmap-topic "two" "three"))))
  (check-equal? (add-parents test-pmap-main) 
                '(pmap-main ((parent "")) (foo ((parent "pmap-main"))) (bar ((parent "pmap-main"))) (one ((parent "pmap-main")) (two ((parent "one")) (three ((parent "two"))))))))

;; remove parents from map (i.e., just remove attrs)
;; is not the inverse of add-parents, i.e., you do not get back your original input.
(define/contract (remove-parents mt) 
  (pmap? . -> . pmap?)
  (remove-attrs mt))

(module+ test
  (check-equal? (remove-parents 
                 '(pmap-main ((parent "")) (foo ((parent ""))) (bar ((parent ""))) 
                             (one ((parent "")) (two ((parent "one")) (three ((parent "two")))))))
                '(pmap-main (foo) (bar) (one (two (three))))))

;; todo: what is this for?
(define/contract (main->pmap main)
  (tagged-xexpr? . -> . pmap?)
  (let-values ([(nx metas) (extract-tag-from-xexpr 'meta main)])
    (add-parents nx)))

(module+ test
  (define mt-pmap `(pmap-main "foo" "bar" ,(pmap-topic "one" (pmap-topic "two" "three")) (meta "foo" "bar")))
  (check-equal? (main->pmap mt-pmap) 
                '(pmap-main ((parent "")) (foo ((parent "pmap-main"))) (bar ((parent "pmap-main"))) (one ((parent "pmap-main")) (two ((parent "one")) (three ((parent "two"))))))))


;; todo: what is this for? to have default input?
(define pmap (main->pmap pmap-main))


(define/contract (pmap-key? x)
  (any/c . -> . boolean?)
  ;; OK for map-key to be #f
  (or (symbol? x) (string? x) (eq? x #f)))

;; return the parent of a given name
(define/contract (parent element [pmap pmap])
  ((pmap-key?) (pmap?) . ->* . (or/c string? boolean?)) 
  (and element (let ([result (se-path* `(,(->symbol element) #:parent) pmap)])
                 (and result (->string result))))) ; se-path* returns #f if nothing found


(module+ test
  (define test-pmap (main->pmap test-pmap-main))
  (check-equal? (parent 'three test-pmap) "two")
  (check-equal? (parent "three" test-pmap) "two")
  (check-false (parent 'nonexistent-name test-pmap)))



; get children of a particular element
(define/contract (children element [pmap pmap])
  ((pmap-key?) (pmap?) . ->* . (or/c list? boolean?))  
  ;; se-path*/list returns '() if nothing found
  (and element  (let ([children (se-path*/list `(,(->symbol element)) pmap)])
                  ; If there are sublists, just take first element
                  (and (not (empty? children)) (map (λ(i) (->string (if (list? i) (car i) i))) children)))))

(module+ test
  (check-equal? (children 'one test-pmap) (list "two"))
  (check-equal? (children 'two test-pmap) (list "three"))
  (check-false (children 'three test-pmap))
  (check-false (children 'fooburger test-pmap)))


;; find all siblings on current level: go up to parent and ask for children
(define/contract (siblings element [pmap pmap])
  ;; this never returns false: element is always a sibling of itself.
  ;; todo: how to use input value in contract? e.g., to check that element is part of output list
  ((pmap-key?) (pmap?) . ->* . (or/c list? boolean?))  
  (children (parent element pmap) pmap))

(module+ test
  (check-equal? (siblings 'one test-pmap) '("foo" "bar" "one"))
  (check-equal? (siblings 'foo test-pmap) '("foo" "bar" "one"))
  (check-equal? (siblings 'two test-pmap) '("two"))
  (check-false (siblings 'invalid-key test-pmap)))

;; helper function
(define/contract (side-siblings side element [pmap pmap]) 
  ((symbol? pmap-key?) (pmap?) . ->* . (or/c list? boolean?))
  (define result ((if (equal? side 'left) takef takef-right) 
                  (siblings element pmap) 
                  (λ(i) (not (equal? (->string element) (->string i))))))
  (and (not (empty? result)) result))


(define/contract (pmap-split element elements)
  (pmap-key? (listof pmap-key?) . -> . (values (listof pmap-key?) (listof pmap-key?)))
  (define-values (left right) (splitf-at elements 
                                         (λ(e) (not (equal? (->string e) (->string element))))))
  (values left (cdr right)))

(module+ test
  (check-equal? (values->list (pmap-split 'bar (siblings 'bar test-pmap))) (list '("foo") '("one"))))


;; siblings to the left of target element (i.e., precede in map order)
(define (left-siblings element [pmap pmap])
  (side-siblings 'left element pmap))

(module+ test
  (check-equal? (left-siblings 'one test-pmap) '("foo" "bar"))
  (check-false (left-siblings 'foo test-pmap)))

;; siblings to the right of target element (i.e., follow in map order)
(define (right-siblings element [pmap pmap])
  (side-siblings 'right element pmap))

(module+ test
  (check-false (right-siblings 'one test-pmap))
  (check-equal? (right-siblings 'foo test-pmap) '("bar" "one")))


;; get element immediately to the left in map
(define/contract (left-sibling element [pmap pmap])
  ((pmap-key?) (pmap?) . ->* . (or/c string? boolean?))
  (define siblings (left-siblings element pmap))
  (and siblings (last siblings)))

(module+ test
  (check-equal? (left-sibling 'bar test-pmap) "foo")
  (check-false (left-sibling 'foo test-pmap)))

;; get element immediately to the right in map
(define/contract (right-sibling element [pmap pmap])
  ((pmap-key?) (pmap?) . ->* . (or/c string? boolean?))
  (define siblings (right-siblings element pmap))
  (and siblings (first siblings)))

(module+ test
  (check-equal? (right-sibling 'foo test-pmap) "bar")
  (check-false (right-sibling 'one test-pmap)))


;; flatten map to sequence
(define/contract (make-page-sequence [pmap pmap])
  (pmap? . -> . (listof string?))
  ; use cdr to get rid of main-map tag at front
  (map ->string (cdr (flatten (remove-parents pmap))))) 

(module+ test
  (check-equal? (make-page-sequence test-pmap) '("foo" "bar" "one" "two" "three")))

;; helper function for get-previous-pages and get-next-pages
(define/contract (adjacent-pages side element [pmap pmap])
  ((symbol? pmap-key?) (pmap?) . ->* . (or/c list? boolean?))
  (define result ((if (equal? side 'left) takef takef-right)
                  (make-page-sequence pmap) (λ(y) (not (equal? (->string element) (->string y))))))
  (and (not (empty? result)) result))

(module+ test
  (check-equal? (adjacent-pages 'left 'one test-pmap) '("foo" "bar"))
  (check-equal? (adjacent-pages 'left 'three test-pmap) '("foo" "bar" "one" "two"))
  (check-false (adjacent-pages 'left 'foo test-pmap)))


;; get sequence of earlier pages
(define/contract (previous-pages element [pmap pmap])
  ((pmap-key?) (pmap?) . ->* . (or/c list? boolean?))
  (adjacent-pages 'left element pmap))

(module+ test
  (check-equal? (previous-pages 'one test-pmap) '("foo" "bar"))
  (check-equal? (previous-pages 'three test-pmap) '("foo" "bar" "one" "two"))
  (check-false (previous-pages 'foo test-pmap)))


;; get sequence of next pages
(define (next-pages element [pmap pmap])
  ((pmap-key?) (pmap?) . ->* . (or/c list? boolean?))
  (adjacent-pages 'right element pmap))

(module+ test
  (check-equal? (next-pages 'foo test-pmap) '("bar" "one" "two" "three"))
  (check-equal? (next-pages 'one test-pmap) '("two" "three"))
  (check-false (next-pages 'three test-pmap)))

;; get page immediately previous
(define/contract (previous-page element [pmap pmap])
  ((pmap-key?) (pmap?) . ->* . (or/c string? boolean?))
  (define result (previous-pages element pmap))
  (and result (last result)))

(module+ test
  (check-equal? (previous-page 'one test-pmap) "bar")
  (check-equal? (previous-page 'three test-pmap) "two")
  (check-false (previous-page 'foo test-pmap)))

;; get page immediately next
(define (next-page element [pmap pmap])
  ((pmap-key?) (pmap?) . ->* . (or/c string? boolean?))
  (define result (next-pages element pmap))
  (and result (first result)))

(module+ test
  (check-equal? (next-page 'foo test-pmap) "bar")
  (check-equal? (next-page 'one test-pmap) "two")
  (check-false (next-page 'three test-pmap)))

(module+ test
  ;; need to parameterize current-directory
  ;; because pollen main depends on it to find the include functions
  (define pm (parameterize ([current-directory "./tests/"])
               (main->pmap (dynamic-require "test-pmap.p" 'main))))
  (check-equal? (previous-page (parent 'printers-and-paper pm) pm) "ligatures"))