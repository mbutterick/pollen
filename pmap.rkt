#lang racket/base
(require xml xml/path racket/list racket/string racket/contract racket/match racket/set)
(require "tools.rkt" "world.rkt" "pmap-decode.rkt")

(module+ test (require rackunit))

(provide (all-defined-out))

;; function to set up the project-pmap.
;; this is to make life simpler when using map navigation functions.
;; the current main.pmap of the project is used as the default input.
;; without this, you'd have to pass it over and over.
;; which is sort of the functional lifestyle, 
;; but in templates, gets tiresome and error-prone.
(define/contract (make-project-pmap)
  (-> pmap?)
  (define pmap-source (build-path START_DIR DEFAULT_POLLEN_MAP))
  (if (file-exists? pmap-source)
      ;; Load it from default path.
      ;; dynamic require of a pmap source file gets you a full pmap. 
      (dynamic-require pmap-source POLLEN_ROOT)
      ;; ... or else synthesize it
      (let* ([files (directory-list START_DIR)]
             ;; restrict files to those with pollen extensions
             [files (map remove-ext (filter (λ(x) (has-ext? x POLLEN_SOURCE_EXT)) files))])
        ;; make a POLLEN_MAP_ROOT_NAME structure and convert it to a full pmap
        (pmap-root->pmap (cons POLLEN_MAP_ROOT_NAME (map path->string files))))))

(define project-pmap (make-project-pmap))


;; remove parents from map (i.e., just remove attrs)
;; is not the inverse of add-parents, i.e., you do not get back your original input.
(define/contract (remove-parents mt) 
  (pmap? . -> . tagged-xexpr?)
  (remove-attrs mt))

(module+ test
  (check-equal? (remove-parents 
                 `(pmap-main ((,POLLEN_MAP_PARENT_KEY "")) (foo ((,POLLEN_MAP_PARENT_KEY ""))) (bar ((,POLLEN_MAP_PARENT_KEY ""))) (one ((,POLLEN_MAP_PARENT_KEY "")) (two ((,POLLEN_MAP_PARENT_KEY "one")) (three ((,POLLEN_MAP_PARENT_KEY "two")))))))
                '(pmap-main (foo) (bar) (one (two (three))))))


(module+ test
  (let ([sample-main `(POLLEN_MAP_ROOT_NAME "foo" "bar" (one (two "three")))])
    (check-equal? (pmap-root->pmap sample-main) 
                  `(POLLEN_MAP_ROOT_NAME ((,POLLEN_MAP_PARENT_KEY "")) (foo ((,POLLEN_MAP_PARENT_KEY "POLLEN_MAP_ROOT_NAME"))) (bar ((,POLLEN_MAP_PARENT_KEY "POLLEN_MAP_ROOT_NAME"))) (one ((,POLLEN_MAP_PARENT_KEY "POLLEN_MAP_ROOT_NAME")) (two ((,POLLEN_MAP_PARENT_KEY "one")) (three ((,POLLEN_MAP_PARENT_KEY "two")))))))))



;; return the parent of a given name
(define/contract (parent element [pmap project-pmap])
  ((pmap-key?) (pmap?) . ->* . (or/c string? boolean?)) 
  (and element (let ([result (se-path* `(,(->symbol element) #:parent) pmap)])
                 (and result (->string result))))) ; se-path* returns #f if nothing found


(module+ test
  (define test-pmap-main `(pmap-main "foo" "bar" (one (two "three"))))
  (define test-pmap (pmap-root->pmap test-pmap-main))
  (check-equal? (parent 'three test-pmap) "two")
  (check-equal? (parent "three" test-pmap) "two")
  (check-false (parent 'nonexistent-name test-pmap)))



; get children of a particular element
(define/contract (children element [pmap project-pmap])
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
(define/contract (siblings element [pmap project-pmap])
  ;; this never returns false: element is always a sibling of itself.
  ;; todo: how to use input value in contract? e.g., to check that element is part of output list
  ((pmap-key?) (pmap?) . ->* . (or/c list? boolean?))  
  (children (parent element pmap) pmap))

(module+ test
  (check-equal? (siblings 'one test-pmap) '("foo" "bar" "one"))
  (check-equal? (siblings 'foo test-pmap) '("foo" "bar" "one"))
  (check-equal? (siblings 'two test-pmap) '("two"))
  (check-false (siblings 'invalid-key test-pmap)))



(define/contract (siblings-split element [pmap project-pmap])
  ((pmap-key?) (pmap?) . ->* . (values (or/c (listof pmap-key?) boolean?) 
                                       (or/c (listof pmap-key?) boolean?)))
  (let-values ([(left right) (splitf-at (siblings element pmap) 
                                        (λ(e) (not (equal? (->string e) (->string element)))))])
    (values (if (empty? left) #f left) (if (empty? (cdr right)) #f (cdr right)))))

(module+ test
  (check-equal? (values->list (siblings-split 'one test-pmap)) '(("foo" "bar") #f))
  (check-equal? (values->list (siblings-split 'bar test-pmap)) (list '("foo") '("one"))))


;; siblings to the left of target element (i.e., precede in map order)
(define (siblings-left element [pmap project-pmap])
  (let-values ([(left right) (siblings-split element pmap)])
    left))

(module+ test
  (check-equal? (siblings-left 'one test-pmap) '("foo" "bar"))
  (check-false (siblings-left 'foo test-pmap)))

;; siblings to the right of target element (i.e., follow in map order)
(define (siblings-right element [pmap project-pmap])
  (let-values ([(left right) (siblings-split element pmap)])
    right))

(module+ test
  (check-false (siblings-right 'one test-pmap))
  (check-equal? (siblings-right 'foo test-pmap) '("bar" "one")))


;; get element immediately to the left in map
(define/contract (sibling-previous element [pmap project-pmap])
  ((pmap-key?) (pmap?) . ->* . (or/c string? boolean?))
  (let ([siblings (siblings-left element pmap)])
    (and siblings (last siblings))))

(module+ test
  (check-equal? (sibling-previous 'bar test-pmap) "foo")
  (check-false (sibling-previous 'foo test-pmap)))

;; get element immediately to the right in map
(define/contract (sibling-next element [pmap project-pmap])
  ((pmap-key?) (pmap?) . ->* . (or/c string? boolean?))
  (let ([siblings (siblings-right element pmap)])
    (and siblings (first siblings))))

(module+ test
  (check-equal? (sibling-next 'foo test-pmap) "bar")
  (check-false (sibling-next 'one test-pmap)))


;; flatten map to sequence
(define/contract (all-pages [pmap project-pmap])
  (pmap? . -> . (listof string?))
  ; use cdr to get rid of main-map tag at front
  (map ->string (cdr (flatten (remove-parents pmap))))) 

(module+ test
  (check-equal? (all-pages test-pmap) '("foo" "bar" "one" "two" "three")))

;; helper function for get-previous-pages and get-next-pages
(define/contract (adjacent-pages side element [pmap project-pmap])
  ((symbol? pmap-key?) (pmap?) . ->* . (or/c list? boolean?))
  (let ([result ((if (equal? side 'left) 
                     takef 
                     takef-right) (all-pages pmap) 
                                  (λ(y) (not (equal? (->string element) (->string y)))))])
    (and (not (empty? result)) result)))

(module+ test
  (check-equal? (adjacent-pages 'left 'one test-pmap) '("foo" "bar"))
  (check-equal? (adjacent-pages 'left 'three test-pmap) '("foo" "bar" "one" "two"))
  (check-false (adjacent-pages 'left 'foo test-pmap)))


;; get sequence of earlier pages
(define/contract (previous-pages element [pmap project-pmap])
  ((pmap-key?) (pmap?) . ->* . (or/c list? boolean?))
  (adjacent-pages 'left element pmap))

(module+ test
  (check-equal? (previous-pages 'one test-pmap) '("foo" "bar"))
  (check-equal? (previous-pages 'three test-pmap) '("foo" "bar" "one" "two"))
  (check-false (previous-pages 'foo test-pmap)))


;; get sequence of next pages
(define (next-pages element [pmap project-pmap])
  ((pmap-key?) (pmap?) . ->* . (or/c list? boolean?))
  (adjacent-pages 'right element pmap))

(module+ test
  (check-equal? (next-pages 'foo test-pmap) '("bar" "one" "two" "three"))
  (check-equal? (next-pages 'one test-pmap) '("two" "three"))
  (check-false (next-pages 'three test-pmap)))

;; get page immediately previous
(define/contract (previous-page element [pmap project-pmap])
  ((pmap-key?) (pmap?) . ->* . (or/c string? boolean?))
  (let ([result (previous-pages element pmap)])
    (and result (last result))))

(module+ test
  (check-equal? (previous-page 'one test-pmap) "bar")
  (check-equal? (previous-page 'three test-pmap) "two")
  (check-false (previous-page 'foo test-pmap)))

;; get page immediately next
(define (next-page element [pmap project-pmap])
  ((pmap-key?) (pmap?) . ->* . (or/c string? boolean?))
  (let ([result (next-pages element pmap)])
    (and result (first result))))

(module+ test
  (check-equal? (next-page 'foo test-pmap) "bar")
  (check-equal? (next-page 'one test-pmap) "two")
  (check-false (next-page 'three test-pmap)))




