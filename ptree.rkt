#lang racket/base
(require xml xml/path racket/list racket/string racket/contract racket/match racket/set)
(require "tools.rkt" "world.rkt" "ptree-decode.rkt" "debug.rkt")

(module+ test (require rackunit))

(provide (all-defined-out))

;; function to set up the project-ptree.
;; this is to make life simpler when using tree navigation functions.
;; the current main.ptree of the project is used as the default input.
;; without this, you'd have to pass it over and over.
;; which is sort of the functional lifestyle, 
;; but in templates, gets tiresome and error-prone.
(define/contract (make-project-ptree)
  (-> ptree?)
  (define ptree-source (build-path START_DIR DEFAULT_POLLEN_TREE))
  (if (file-exists? ptree-source)
      ;; Load it from default path.
      ;; dynamic require of a ptree source file gets you a full ptree. 
      (begin
        (message "Loading ptree file" (->string ptree-source))
        (dynamic-require ptree-source POLLEN_ROOT))
      ;; ... or else synthesize it
      (let* ([files (directory-list START_DIR)]
             ;; restrict files to those with pollen extensions
             [files (map remove-ext (filter (λ(x) (has-ext? x POLLEN_SOURCE_EXT)) files))])
        ;; make a POLLEN_TREE_ROOT_NAME structure and convert it to a full ptree
        (message "Generating ptree from file listing")
        (ptree-root->ptree (cons POLLEN_TREE_ROOT_NAME (map path->pnode files))))))

;; remove parents from tree (i.e., just remove attrs)
;; is not the inverse of add-parents, i.e., you do not get back your original input.
(define/contract (remove-parents mt) 
  (ptree? . -> . tagged-xexpr?)
  (remove-attrs mt))

(module+ test
  (check-equal? (remove-parents 
                 `(ptree-main ((,POLLEN_TREE_PARENT_NAME "")) (foo ((,POLLEN_TREE_PARENT_NAME ""))) (bar ((,POLLEN_TREE_PARENT_NAME ""))) (one ((,POLLEN_TREE_PARENT_NAME "")) (two ((,POLLEN_TREE_PARENT_NAME "one")) (three ((,POLLEN_TREE_PARENT_NAME "two")))))))
                '(ptree-main (foo) (bar) (one (two (three))))))


(module+ test
  (let ([sample-main `(POLLEN_TREE_ROOT_NAME "foo" "bar" (one (two "three")))])
    (check-equal? (ptree-root->ptree sample-main) 
                  `(POLLEN_TREE_ROOT_NAME ((,POLLEN_TREE_PARENT_NAME "")) (foo ((,POLLEN_TREE_PARENT_NAME "POLLEN_TREE_ROOT_NAME"))) (bar ((,POLLEN_TREE_PARENT_NAME "POLLEN_TREE_ROOT_NAME"))) (one ((,POLLEN_TREE_PARENT_NAME "POLLEN_TREE_ROOT_NAME")) (two ((,POLLEN_TREE_PARENT_NAME "one")) (three ((,POLLEN_TREE_PARENT_NAME "two")))))))))



;; return the parent of a given name
(define/contract (parent node [ptree project-ptree])
  ((pnode?) (ptree?) . ->* . (or/c string? boolean?)) 
  (and node (let ([result (se-path* `(,(->symbol node) #:parent) ptree)])
                 (and result (->string result))))) ; se-path* returns #f if nothing found


(module+ test
  (define test-ptree-main `(ptree-main "foo" "bar" (one (two "three"))))
  (define test-ptree (ptree-root->ptree test-ptree-main))
  (check-equal? (parent 'three test-ptree) "two")
  (check-equal? (parent "three" test-ptree) "two")
  (check-false (parent 'nonexistent-name test-ptree)))



; get children of a particular node
(define/contract (children node [ptree project-ptree])
  ((pnode?) (ptree?) . ->* . (or/c list? boolean?))  
  ;; se-path*/list returns '() if nothing found
  (and node  (let ([children (se-path*/list `(,(->symbol node)) ptree)])
                  ; If there are sublists, just take first node
                  (and (not (empty? children)) (map (λ(i) (->string (if (list? i) (car i) i))) children)))))

(module+ test
  (check-equal? (children 'one test-ptree) (list "two"))
  (check-equal? (children 'two test-ptree) (list "three"))
  (check-false (children 'three test-ptree))
  (check-false (children 'fooburger test-ptree)))


;; find all siblings on current level: go up to parent and ask for children
(define/contract (siblings node [ptree project-ptree])
  ;; this never returns false: node is always a sibling of itself.
  ;; todo: how to use input value in contract? e.g., to check that node is part of output list
  ((pnode?) (ptree?) . ->* . (or/c list? boolean?))  
  (children (parent node ptree) ptree))

(module+ test
  (check-equal? (siblings 'one test-ptree) '("foo" "bar" "one"))
  (check-equal? (siblings 'foo test-ptree) '("foo" "bar" "one"))
  (check-equal? (siblings 'two test-ptree) '("two"))
  (check-false (siblings 'invalid-key test-ptree)))



(define/contract (siblings-split node [ptree project-ptree])
  ((pnode?) (ptree?) . ->* . (values (or/c (listof pnode?) boolean?) 
                                         (or/c (listof pnode?) boolean?)))
  (let-values ([(left right) (splitf-at (siblings node ptree) 
                                        (λ(e) (not (equal? (->string e) (->string node)))))])
    (values (if (empty? left) #f left) (if (empty? (cdr right)) #f (cdr right)))))

(module+ test
  (check-equal? (values->list (siblings-split 'one test-ptree)) '(("foo" "bar") #f))
  (check-equal? (values->list (siblings-split 'bar test-ptree)) (list '("foo") '("one"))))


;; siblings to the left of target node (i.e., precede in tree order)
(define (siblings-left node [ptree project-ptree])
  (let-values ([(left right) (siblings-split node ptree)])
    left))

(module+ test
  (check-equal? (siblings-left 'one test-ptree) '("foo" "bar"))
  (check-false (siblings-left 'foo test-ptree)))

;; siblings to the right of target node (i.e., follow in tree order)
(define (siblings-right node [ptree project-ptree])
  (let-values ([(left right) (siblings-split node ptree)])
    right))

(module+ test
  (check-false (siblings-right 'one test-ptree))
  (check-equal? (siblings-right 'foo test-ptree) '("bar" "one")))


;; get node immediately to the left in tree
(define/contract (sibling-previous node [ptree project-ptree])
  ((pnode?) (ptree?) . ->* . (or/c string? boolean?))
  (let ([siblings (siblings-left node ptree)])
    (and siblings (last siblings))))

(module+ test
  (check-equal? (sibling-previous 'bar test-ptree) "foo")
  (check-false (sibling-previous 'foo test-ptree)))

;; get node immediately to the right in tree
(define/contract (sibling-next node [ptree project-ptree])
  ((pnode?) (ptree?) . ->* . (or/c string? boolean?))
  (let ([siblings (siblings-right node ptree)])
    (and siblings (first siblings))))

(module+ test
  (check-equal? (sibling-next 'foo test-ptree) "bar")
  (check-false (sibling-next 'one test-ptree)))


;; flatten tree to sequence
(define/contract (all-pages [ptree project-ptree])
  (ptree? . -> . (listof string?))
  ; use cdr to get rid of root tag at front
  (map ->string (cdr (flatten (remove-parents ptree))))) 

(module+ test
  (check-equal? (all-pages test-ptree) '("foo" "bar" "one" "two" "three")))

;; helper function for get-previous-pages and get-next-pages
(define/contract (adjacent-pages side node [ptree project-ptree])
  ((symbol? pnode?) (ptree?) . ->* . (or/c list? boolean?))
  (let ([result ((if (equal? side 'left) 
                     takef 
                     takef-right) (all-pages ptree) 
                                  (λ(y) (not (equal? (->string node) (->string y)))))])
    (and (not (empty? result)) result)))

(module+ test
  (check-equal? (adjacent-pages 'left 'one test-ptree) '("foo" "bar"))
  (check-equal? (adjacent-pages 'left 'three test-ptree) '("foo" "bar" "one" "two"))
  (check-false (adjacent-pages 'left 'foo test-ptree)))


;; get sequence of earlier pages
(define/contract (previous-pages node [ptree project-ptree])
  ((pnode?) (ptree?) . ->* . (or/c list? boolean?))
  (adjacent-pages 'left node ptree))

(module+ test
  (check-equal? (previous-pages 'one test-ptree) '("foo" "bar"))
  (check-equal? (previous-pages 'three test-ptree) '("foo" "bar" "one" "two"))
  (check-false (previous-pages 'foo test-ptree)))


;; get sequence of next pages
(define (next-pages node [ptree project-ptree])
  ((pnode?) (ptree?) . ->* . (or/c list? boolean?))
  (adjacent-pages 'right node ptree))

(module+ test
  (check-equal? (next-pages 'foo test-ptree) '("bar" "one" "two" "three"))
  (check-equal? (next-pages 'one test-ptree) '("two" "three"))
  (check-false (next-pages 'three test-ptree)))

;; get page immediately previous
(define/contract (previous-page node [ptree project-ptree])
  ((pnode?) (ptree?) . ->* . (or/c string? boolean?))
  (let ([result (previous-pages node ptree)])
    (and result (last result))))

(module+ test
  (check-equal? (previous-page 'one test-ptree) "bar")
  (check-equal? (previous-page 'three test-ptree) "two")
  (check-false (previous-page 'foo test-ptree)))

;; get page immediately next
(define (next-page node [ptree project-ptree])
  ((pnode?) (ptree?) . ->* . (or/c string? boolean?))
  (let ([result (next-pages node ptree)])
    (and result (first result))))

(module+ test
  (check-equal? (next-page 'foo test-ptree) "bar")
  (check-equal? (next-page 'one test-ptree) "two")
  (check-false (next-page 'three test-ptree)))

;; convert path to pnode
;; used for converting "here" values to pnodes
(define/contract (path->pnode x)
  (pathish? . -> . pnode?)
  (->string (remove-all-ext (last (explode-path (->path x))))))

(module+ test
  (check-equal? (path->pnode "bar") "bar")
  (check-equal? (path->pnode "foo/bar") "bar")
  (check-equal? (path->pnode "foo/bar.html") "bar")
  (check-equal? (path->pnode "/Users/this/that/foo/bar.html.pp") "bar"))

(define here->pnode path->pnode)

;; convert key to URL
;; = key name + suffix of template (or suffix of default template)
;; todo: finish this function, right now it just appends html
;; this would also be useful for start page (showing correct url of generated pages)
(define/contract (pnode->url key)
  (pnode? . -> . string?)
  (string-append key ".html"))


;; this project setup must follow definitions to prevent undefined errors
(define project-ptree (make-project-ptree))

