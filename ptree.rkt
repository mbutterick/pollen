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
    (ptree-root->ptree (cons POLLEN_TREE_ROOT_NAME (map path->name files)))))


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
(define/contract (parent name [ptree current-ptree])
  ((ptree-name?) (ptree?) . ->* . (or/c string? boolean?)) 
  (and name (let ([result (se-path* `(,(->symbol name) #:parent) ptree)])
               (and result (->string result))))) ; se-path* returns #f if nothing found


(define ptree-parent parent)

(module+ test
  (define test-ptree-main `(ptree-main "foo" "bar" (one (two "three"))))
  (define test-ptree (ptree-root->ptree test-ptree-main))
  (check-equal? (parent 'three test-ptree) "two")
  (check-equal? (parent "three" test-ptree) "two")
  (check-false (parent 'nonexistent-name test-ptree)))



; get children of a particular name
(define/contract (children name [ptree current-ptree])
  ((ptree-name?) (ptree?) . ->* . (or/c list? boolean?))  
  ;; se-path*/list returns '() if nothing found
  (and name  (let ([children (se-path*/list `(,(->symbol name)) ptree)])
                ; If there are sublists, just take first name
                (and (not (empty? children)) (map (λ(i) (->string (if (list? i) (car i) i))) children)))))

(module+ test
  (check-equal? (children 'one test-ptree) (list "two"))
  (check-equal? (children 'two test-ptree) (list "three"))
  (check-false (children 'three test-ptree))
  (check-false (children 'fooburger test-ptree)))


;; find all siblings on current level: go up to parent and ask for children
(define/contract (siblings name [ptree current-ptree])
  ;; this never returns false: name is always a sibling of itself.
  ;; todo: how to use input value in contract? e.g., to check that name is part of output list
  ((ptree-name?) (ptree?) . ->* . (or/c list? boolean?))  
  (children (parent name ptree) ptree))

(module+ test
  (check-equal? (siblings 'one test-ptree) '("foo" "bar" "one"))
  (check-equal? (siblings 'foo test-ptree) '("foo" "bar" "one"))
  (check-equal? (siblings 'two test-ptree) '("two"))
  (check-false (siblings 'invalid-key test-ptree)))



(define/contract (siblings-split name [ptree current-ptree])
  ((ptree-name?) (ptree?) . ->* . (values (or/c (listof ptree-name?) boolean?) 
                                     (or/c (listof ptree-name?) boolean?)))
  (let-values ([(left right) (splitf-at (siblings name ptree) 
                                        (λ(e) (not (equal? (->string e) (->string name)))))])
    (values (if (empty? left) #f left) (if (empty? (cdr right)) #f (cdr right)))))

(module+ test
  (check-equal? (values->list (siblings-split 'one test-ptree)) '(("foo" "bar") #f))
  (check-equal? (values->list (siblings-split 'bar test-ptree)) (list '("foo") '("one"))))


;; siblings to the left of target name (i.e., precede in tree order)
(define (siblings-left name [ptree current-ptree])
  (let-values ([(left right) (siblings-split name ptree)])
    left))

(module+ test
  (check-equal? (siblings-left 'one test-ptree) '("foo" "bar"))
  (check-false (siblings-left 'foo test-ptree)))

;; siblings to the right of target name (i.e., follow in tree order)
(define (siblings-right name [ptree current-ptree])
  (let-values ([(left right) (siblings-split name ptree)])
    right))

(module+ test
  (check-false (siblings-right 'one test-ptree))
  (check-equal? (siblings-right 'foo test-ptree) '("bar" "one")))


;; get name immediately to the left in tree
(define/contract (sibling-previous name [ptree current-ptree])
  ((ptree-name?) (ptree?) . ->* . (or/c string? boolean?))
  (let ([siblings (siblings-left name ptree)])
    (and siblings (last siblings))))

(module+ test
  (check-equal? (sibling-previous 'bar test-ptree) "foo")
  (check-false (sibling-previous 'foo test-ptree)))

;; get name immediately to the right in tree
(define/contract (sibling-next name [ptree current-ptree])
  ((ptree-name?) (ptree?) . ->* . (or/c string? boolean?))
  (let ([siblings (siblings-right name ptree)])
    (and siblings (first siblings))))

(module+ test
  (check-equal? (sibling-next 'foo test-ptree) "bar")
  (check-false (sibling-next 'one test-ptree)))


;; flatten tree to sequence
(define/contract (all-names [ptree current-ptree])
  (ptree? . -> . (listof string?))
  ; use cdr to get rid of root tag at front
  (map ->string (cdr (flatten (remove-parents ptree))))) 

(module+ test
  (check-equal? (all-names test-ptree) '("foo" "bar" "one" "two" "three")))

;; helper function for get-previous-names and get-next-names
(define/contract (adjacent-names side name [ptree current-ptree])
  ((symbol? ptree-name?) (ptree?) . ->* . (or/c list? boolean?))
  (let ([result ((if (equal? side 'left) 
                     takef 
                     takef-right) (all-names ptree) 
                                  (λ(y) (not (equal? (->string name) (->string y)))))])
    (and (not (empty? result)) result)))

(module+ test
  (check-equal? (adjacent-names 'left 'one test-ptree) '("foo" "bar"))
  (check-equal? (adjacent-names 'left 'three test-ptree) '("foo" "bar" "one" "two"))
  (check-false (adjacent-names 'left 'foo test-ptree)))


;; get sequence of earlier names
(define/contract (ptree-previous* name [ptree current-ptree])
  ((ptree-name?) (ptree?) . ->* . (or/c list? boolean?))
  (adjacent-names 'left name ptree))

(module+ test
  (check-equal? (ptree-previous* 'one test-ptree) '("foo" "bar"))
  (check-equal? (ptree-previous* 'three test-ptree) '("foo" "bar" "one" "two"))
  (check-false (ptree-previous* 'foo test-ptree)))


;; get sequence of next names
(define (ptree-next* name [ptree current-ptree])
  ((ptree-name?) (ptree?) . ->* . (or/c list? boolean?))
  (adjacent-names 'right name ptree))

(module+ test
  (check-equal? (ptree-next* 'foo test-ptree) '("bar" "one" "two" "three"))
  (check-equal? (ptree-next* 'one test-ptree) '("two" "three"))
  (check-false (ptree-next* 'three test-ptree)))

;; get name immediately previous
(define/contract (ptree-previous name [ptree current-ptree])
  ((ptree-name?) (ptree?) . ->* . (or/c string? boolean?))
  (let ([result (ptree-previous* name ptree)])
    (and result (last result))))

(module+ test
  (check-equal? (ptree-previous 'one test-ptree) "bar")
  (check-equal? (ptree-previous 'three test-ptree) "two")
  (check-false (ptree-previous 'foo test-ptree)))

;; get name immediately next
(define (ptree-next name [ptree current-ptree])
  ((ptree-name?) (ptree?) . ->* . (or/c string? boolean?))
  (let ([result (ptree-next* name ptree)])
    (and result (first result))))

(module+ test
  (check-equal? (ptree-next 'foo test-ptree) "bar")
  (check-equal? (ptree-next 'one test-ptree) "two")
  (check-false (ptree-next 'three test-ptree)))

;; convert path to name
;; used for converting "here" values to names
(define/contract (path->name x)
  (pathish? . -> . ptree-name?)
  (->string (remove-all-ext (last (explode-path (->path x))))))

(module+ test
  (check-equal? (path->name "bar") "bar")
  (check-equal? (path->name "foo/bar") "bar")
  (check-equal? (path->name "foo/bar.html") "bar")
  (check-equal? (path->name "/Users/this/that/foo/bar.html.pp") "bar"))

(define here->name path->name)

(define/contract (name->url name [files current-url-context])
  ((ptree-name?) ((listof pathish?)) . ->* . (or/c string? boolean?))
  ;; upconvert all files to their output path
  ;; then remove duplicates because some sources might have already been rendered
  (define output-paths (remove-duplicates (map ->output-path files) equal?))
  ;; find ones that match name
  (define matching-paths (filter (λ(x) (equal? (path->name x) (->string name))) output-paths))
  
  (cond
    [((len matching-paths) . = . 1) (->string (car matching-paths))]
    [((len matching-paths) . > . 1) (error "More than one matching URL for" name)]
    [else #f] ))

(define ptree-name->url name->url)


(module+ test
  (define files '("foo.html" "bar.html" "bar.html.p" "zap.html" "zap.xml"))
  (check-equal? (name->url 'foo files) "foo.html")
  (check-equal? (name->url 'bar files) "bar.html")
  ;;  (check-equal? (name->url 'zap files) 'error) ;; todo: how to test error?
  (check-false (name->url 'hee files)))


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
(define/contract (valid-names? x)
  (any/c . -> . boolean?)
  (andmap (λ(x) (ptree-name? #:loud #t x)) (filter-not whitespace? (flatten x))))

;; contract for ptree-source-decode
(define/contract (unique-names? x)
  (any/c . -> . boolean?)
  ;; use map ->string to make keys comparable
  (elements-unique? #:loud #t (map ->string (filter-not whitespace? (flatten x)))))


(define/contract (ptree-source-decode . elements)
  (() #:rest (and/c valid-names? unique-names?) . ->* . ptree?)
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
                              (visible-files (->path x)))))

;; set the state variable using the setter
(set-current-url-context pollen-project-directory)

(module+ main
  (displayln "Running module main")
  (set-current-ptree (make-project-ptree (->path "/Users/MB/git/bpt/")))
  (set-current-url-context "/Users/MB/git/bpt/")
  (name->url (ptree-previous (ptree-previous 'what-is-typography))))
