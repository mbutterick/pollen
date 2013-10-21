#lang racket/base
(require racket/contract racket/match xml/path)
(require "tools.rkt" "world.rkt" "debug.rkt" "decode.rkt")

(module+ test (require rackunit))

(provide (all-defined-out))

;; Load ptree file & return ptree
(define/contract (ptree-source->ptree path)
  (pathish? . -> . ptree?)
  ;; dynamic require of a ptree source file gets you a full ptree. 
  (message "Loading ptree file" (->string (file-name-from-path path)))
  (dynamic-require path POLLEN_ROOT))

;; Synthesize ptree from directory listing.
;; Fallback in case ptree file isn't available.
(define/contract (directory->ptree dir)
  (directory-pathish? . -> . ptree?)
  (let ([files (map remove-ext (filter (λ(x) (has-ext? x POLLEN_SOURCE_EXT)) (directory-list dir)))])
    (message "Generating ptree from file listing")
    (ptree-root->ptree (cons POLLEN_TREE_ROOT_NAME (map path->pnode files)))))


;; Try loading from ptree file, or failing that, synthesize ptree.
(define/contract (make-project-ptree [project-dir pollen-project-directory])
  (() (directory-pathish?) . ->* . ptree?)
  (define ptree-source (build-path project-dir DEFAULT_POLLEN_TREE))
  (if (file-exists? ptree-source)
      (ptree-source->ptree ptree-source)
      (directory->ptree project-dir)))

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
(define/contract (parent pnode [ptree current-ptree])
  ((pnode?) (ptree?) . ->* . (or/c string? boolean?)) 
  (and pnode (let ([result (se-path* `(,(->symbol pnode) #:parent) ptree)])
               (and result (->string result))))) ; se-path* returns #f if nothing found


(module+ test
  (define test-ptree-main `(ptree-main "foo" "bar" (one (two "three"))))
  (define test-ptree (ptree-root->ptree test-ptree-main))
  (check-equal? (parent 'three test-ptree) "two")
  (check-equal? (parent "three" test-ptree) "two")
  (check-false (parent 'nonexistent-name test-ptree)))



; get children of a particular pnode
(define/contract (children pnode [ptree current-ptree])
  ((pnode?) (ptree?) . ->* . (or/c list? boolean?))  
  ;; se-path*/list returns '() if nothing found
  (and pnode  (let ([children (se-path*/list `(,(->symbol pnode)) ptree)])
                ; If there are sublists, just take first pnode
                (and (not (empty? children)) (map (λ(i) (->string (if (list? i) (car i) i))) children)))))

(module+ test
  (check-equal? (children 'one test-ptree) (list "two"))
  (check-equal? (children 'two test-ptree) (list "three"))
  (check-false (children 'three test-ptree))
  (check-false (children 'fooburger test-ptree)))


;; find all siblings on current level: go up to parent and ask for children
(define/contract (siblings pnode [ptree current-ptree])
  ;; this never returns false: pnode is always a sibling of itself.
  ;; todo: how to use input value in contract? e.g., to check that pnode is part of output list
  ((pnode?) (ptree?) . ->* . (or/c list? boolean?))  
  (children (parent pnode ptree) ptree))

(module+ test
  (check-equal? (siblings 'one test-ptree) '("foo" "bar" "one"))
  (check-equal? (siblings 'foo test-ptree) '("foo" "bar" "one"))
  (check-equal? (siblings 'two test-ptree) '("two"))
  (check-false (siblings 'invalid-key test-ptree)))



(define/contract (siblings-split pnode [ptree current-ptree])
  ((pnode?) (ptree?) . ->* . (values (or/c (listof pnode?) boolean?) 
                                     (or/c (listof pnode?) boolean?)))
  (let-values ([(left right) (splitf-at (siblings pnode ptree) 
                                        (λ(e) (not (equal? (->string e) (->string pnode)))))])
    (values (if (empty? left) #f left) (if (empty? (cdr right)) #f (cdr right)))))

(module+ test
  (check-equal? (values->list (siblings-split 'one test-ptree)) '(("foo" "bar") #f))
  (check-equal? (values->list (siblings-split 'bar test-ptree)) (list '("foo") '("one"))))


;; siblings to the left of target pnode (i.e., precede in tree order)
(define (siblings-left pnode [ptree current-ptree])
  (let-values ([(left right) (siblings-split pnode ptree)])
    left))

(module+ test
  (check-equal? (siblings-left 'one test-ptree) '("foo" "bar"))
  (check-false (siblings-left 'foo test-ptree)))

;; siblings to the right of target pnode (i.e., follow in tree order)
(define (siblings-right pnode [ptree current-ptree])
  (let-values ([(left right) (siblings-split pnode ptree)])
    right))

(module+ test
  (check-false (siblings-right 'one test-ptree))
  (check-equal? (siblings-right 'foo test-ptree) '("bar" "one")))


;; get pnode immediately to the left in tree
(define/contract (sibling-previous pnode [ptree current-ptree])
  ((pnode?) (ptree?) . ->* . (or/c string? boolean?))
  (let ([siblings (siblings-left pnode ptree)])
    (and siblings (last siblings))))

(module+ test
  (check-equal? (sibling-previous 'bar test-ptree) "foo")
  (check-false (sibling-previous 'foo test-ptree)))

;; get pnode immediately to the right in tree
(define/contract (sibling-next pnode [ptree current-ptree])
  ((pnode?) (ptree?) . ->* . (or/c string? boolean?))
  (let ([siblings (siblings-right pnode ptree)])
    (and siblings (first siblings))))

(module+ test
  (check-equal? (sibling-next 'foo test-ptree) "bar")
  (check-false (sibling-next 'one test-ptree)))


;; flatten tree to sequence
(define/contract (all-pnodes [ptree current-ptree])
  (ptree? . -> . (listof string?))
  ; use cdr to get rid of root tag at front
  (map ->string (cdr (flatten (remove-parents ptree))))) 

(module+ test
  (check-equal? (all-pnodes test-ptree) '("foo" "bar" "one" "two" "three")))

;; helper function for get-previous-pnodes and get-next-pnodes
(define/contract (adjacent-pnodes side pnode [ptree current-ptree])
  ((symbol? pnode?) (ptree?) . ->* . (or/c list? boolean?))
  (let ([result ((if (equal? side 'left) 
                     takef 
                     takef-right) (all-pnodes ptree) 
                                  (λ(y) (not (equal? (->string pnode) (->string y)))))])
    (and (not (empty? result)) result)))

(module+ test
  (check-equal? (adjacent-pnodes 'left 'one test-ptree) '("foo" "bar"))
  (check-equal? (adjacent-pnodes 'left 'three test-ptree) '("foo" "bar" "one" "two"))
  (check-false (adjacent-pnodes 'left 'foo test-ptree)))


;; get sequence of earlier pnodes
(define/contract (previous-pnodes pnode [ptree current-ptree])
  ((pnode?) (ptree?) . ->* . (or/c list? boolean?))
  (adjacent-pnodes 'left pnode ptree))

(module+ test
  (check-equal? (previous-pnodes 'one test-ptree) '("foo" "bar"))
  (check-equal? (previous-pnodes 'three test-ptree) '("foo" "bar" "one" "two"))
  (check-false (previous-pnodes 'foo test-ptree)))


;; get sequence of next pnodes
(define (next-pnodes pnode [ptree current-ptree])
  ((pnode?) (ptree?) . ->* . (or/c list? boolean?))
  (adjacent-pnodes 'right pnode ptree))

(module+ test
  (check-equal? (next-pnodes 'foo test-ptree) '("bar" "one" "two" "three"))
  (check-equal? (next-pnodes 'one test-ptree) '("two" "three"))
  (check-false (next-pnodes 'three test-ptree)))

;; get pnode immediately previous
(define/contract (previous-pnode pnode [ptree current-ptree])
  ((pnode?) (ptree?) . ->* . (or/c string? boolean?))
  (let ([result (previous-pnodes pnode ptree)])
    (and result (last result))))

(module+ test
  (check-equal? (previous-pnode 'one test-ptree) "bar")
  (check-equal? (previous-pnode 'three test-ptree) "two")
  (check-false (previous-pnode 'foo test-ptree)))

;; get pnode immediately next
(define (next-pnode pnode [ptree current-ptree])
  ((pnode?) (ptree?) . ->* . (or/c string? boolean?))
  (let ([result (next-pnodes pnode ptree)])
    (and result (first result))))

(module+ test
  (check-equal? (next-pnode 'foo test-ptree) "bar")
  (check-equal? (next-pnode 'one test-ptree) "two")
  (check-false (next-pnode 'three test-ptree)))

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

(define/contract (pnode->url pnode [files current-url-context])
  ((pnode?) ((listof pathish?)) . ->* . (or/c string? boolean?))
  ;; upconvert all files to their output path
  ;; then remove duplicates because some sources might have already been rendered
  (define output-paths (remove-duplicates (map ->output-path files) equal?))
  ;; find ones that match pnode
  (define matching-paths (filter (λ(x) (equal? (->string (remove-all-ext x)) (->string pnode))) output-paths))
  (cond
    [((len matching-paths) . = . 1) (->string (car matching-paths))]
    [((len matching-paths) . > . 1) (error "More than one matching URL for" pnode)]
    [else #f] ))


(module+ test
  (define files '("foo.html" "bar.html" "bar.html.p" "zap.html" "zap.xml"))
  (check-equal? (pnode->url 'foo files) "foo.html")
  (check-equal? (pnode->url 'bar files) "bar.html")
  ;;  (check-equal? (pnode->url 'zap files) 'error) ;; todo: how to test error?
  (check-equal? (pnode->url 'hee files) "#"))


;; recursively processes tree, converting tree locations & their parents into xexprs of this shape:
;; '(location ((parent "parent")))
(define/contract (add-parents x [parent empty])
  ((tagged-xexpr?) (xexpr-tag?) . ->* . ptree?)
  (match x
    ;; this pattern signifies next level in hierarchy 
    ;; where first element is new parent, and rest are children.
    [(list (? xexpr-tag? next-parent) children ...)
     (let-values ([(tag attr _) (break-tagged-xexpr (add-parents next-parent parent))])
       ;; xexpr with tag as name, parent as attr, children as elements with tag as next parent
       (make-tagged-xexpr tag attr (map (λ(c) (add-parents c tag)) children)))]
    ;; single map entry: convert to xexpr with parent
    [else (make-tagged-xexpr (->symbol x) (make-xexpr-attr POLLEN_TREE_PARENT_NAME (->string parent)))]))


;; this sets default input for following functions
(define/contract (ptree-root->ptree tx)
  ;; (not/c ptree) prevents ptrees from being accepted as input
  ((and/c tagged-xexpr? (not/c ptree?)) . -> . ptree?)
  (add-parents tx))


(module+ test
  (set! test-ptree-main `(ptree-main "foo" "bar" (one (two "three"))))
  (check-equal? (ptree-root->ptree test-ptree-main) 
                `(ptree-main ((,POLLEN_TREE_PARENT_NAME "")) (foo ((,POLLEN_TREE_PARENT_NAME "ptree-main"))) (bar ((,POLLEN_TREE_PARENT_NAME "ptree-main"))) (one ((,POLLEN_TREE_PARENT_NAME "ptree-main")) (two ((,POLLEN_TREE_PARENT_NAME "one")) (three ((,POLLEN_TREE_PARENT_NAME "two"))))))))



;; contract for ptree-source-decode
(define/contract (valid-pnodes? x)
  (any/c . -> . boolean?)
  (andmap (λ(x) (pnode? #:loud #t x)) (filter-not whitespace? (flatten x))))

;; contract for ptree-source-decode
(define/contract (unique-pnodes? x)
  (any/c . -> . boolean?)
  ;; use map ->string to make keys comparable
  (elements-unique? #:loud #t (map ->string (filter-not whitespace? (flatten x)))))


(define/contract (ptree-source-decode . elements)
  (() #:rest (and/c valid-pnodes? unique-pnodes?) . ->* . ptree?)
  (ptree-root->ptree (decode (cons POLLEN_TREE_ROOT_NAME elements)
                             #:xexpr-elements-proc (λ(xs) (filter-not whitespace? xs)))))



(define current-ptree '(empty ((parent "")))) ;; simplest empty ptree that will meet ptree contract

(define/contract (set-current-ptree ptree)
  (ptree? . -> . void?)
  (set! current-ptree ptree))

;; create the state variable
(define current-url-context '())

;; create the state variable setter
(define/contract (set-current-url-context x)
  ((or/c directory-pathish? (listof pathish?)) . -> . void)
  ;; try treating x as a directory, 
  ;; otherwise treat it as a list of paths
  (set! current-url-context (with-handlers ([exn:fail? (λ(e) x)])
                              (directory-list x))))

;; set the state variable using the setter
(set-current-url-context pollen-project-directory)

(module+ main
  (displayln "Running module main")
  (set-current-ptree (make-project-ptree (->path "/Users/MB/git/bpt/")))
  (set-current-url-context (directory-list "/Users/MB/git/bpt/"))
  (pnode->url (previous-pnode (previous-pnode 'what-is-typography))))