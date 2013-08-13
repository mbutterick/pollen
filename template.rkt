#lang racket/base
(require xml xml/path racket/list racket/string racket/contract racket/match)
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
  (when (equal? parent 'map-main) (set! parent empty)) 
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
  (define test-map `(map-main "foo" ,(map-topic "one" (map-topic "two" "three"))))
  (check-equal? (add-parents test-map) 
                '(map-main ((parent "")) (foo ((parent ""))) (one ((parent "")) 
                                                                  (two ((parent "one")) (three ((parent "two"))))))))

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
                 '(map-main ((parent "")) (foo ((parent ""))) 
                            (one ((parent "")) (two ((parent "one")) (three ((parent "two")))))))
                '(map-main (foo) (one (two (three))))))

;; todo: what is this for?
(define (main->tree main)
  (add-parents main))

;; todo: what is this for? to have default input?
(define tree (main->tree map-main))


;; return the parent of a given name
(define/contract (get-parent element [tree tree])
  (((λ(i) (or (symbol? i) (string? i)))) (pmap-tree?) . ->* . (or/c string? boolean?)) 
  (and element (let ([result (se-path* `(,(->symbol element) #:parent) tree)])
                 (and result (->string result))))) ; se-path* returns #f if nothing found


(module+ test
  (define test-tree (main->tree test-map))
  (check-equal? (get-parent 'three test-tree) "two")
  (check-equal? (get-parent "three" test-tree) "two")
  (check-false (get-parent 'fooburger test-tree)))



; algorithm to find children
(define/contract (get-children element [tree tree])
  (((λ(i) (or (symbol? i) (string? i)))) (pmap-tree?) . ->* . (or/c list? boolean?)) 
  ;; find contents of node. 
  ;; se-path*/list returns '() if nothing found
  (and element  (let ([children (se-path*/list `(,(->symbol element)) tree)])
                  ; If there are sublists, just take first element
                  (and (not (empty? children)) (map (λ(i) (->string (if (list? i) (car i) i))) children)))))


(module+ test
  (check-equal? (get-children 'one test-tree) (list "two"))
  (check-equal? (get-children 'two test-tree) (list "three"))
  (check-false (get-children 'three test-tree))
  (check-false (get-children 'fooburger test-tree)))


;; todo next

; find all siblings on current level: go up to parent and ask for children
(define (get-all-siblings x [tree tree])
  (get-children (get-parent x tree) tree))

(define (get-adjacent-siblings x [tree tree])
  (define-values (left right)
    (splitf-at (get-all-siblings x tree) (λ(y) (not (equal? (->string x) (->string y))))))
  ; use cdr because right piece includes x itself at front
  (values left (if (empty? right)
                   empty
                   (cdr right))))

(define (get-left-siblings x [tree tree])
  (define-values (left right) (get-adjacent-siblings x tree))
  left)

(define (get-right-siblings x [tree tree])
  (define-values (left right) (get-adjacent-siblings x tree))
  right)

(define (get-left x [tree tree])
  (if (empty? (get-left-siblings x tree))
      empty
      (last (get-left-siblings x tree))))

(define (get-right x [tree tree])
  (if (empty? (get-right-siblings x tree))
      empty
      (first (get-right-siblings x tree))))


(define (make-page-sequence [tree tree])
  ; use cdr to get rid of body tag at front
  ; todo: calculate exclusions?
  (map ->string (cdr (flatten (remove-parents tree))))) 

(define (get-adjacent-pages x [tree tree])
  (define-values (left right)
    (splitf-at (make-page-sequence tree) (λ(y) (not (equal? (->string x) (->string y))))))
  ; use cdr because right piece includes x itself at front
  (values left (if (empty? right)
                   empty
                   (cdr right))))

(define (get-previous-pages x [tree tree])
  (define-values (left right) (get-adjacent-pages x tree))
  left)

(define (get-next-pages x [tree tree])
  (define-values (left right) (get-adjacent-pages x tree))
  right)

(define (get-previous x [tree tree])
  (if (empty? (get-previous-pages x tree))
      empty
      (last (get-previous-pages x tree))))

(define (get-next x [tree tree])
  (if (empty? (get-next-pages x tree))
      empty
      (first (get-next-pages x tree))))



(provide (all-defined-out) (all-from-out web-server/templates))