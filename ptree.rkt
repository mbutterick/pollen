#lang racket/base
(require racket/contract racket/match xml/path racket/bool)
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
  (let ([files (map remove-ext (filter (λ(x) (has-ext? x POLLEN_DECODER_EXT)) (directory-list dir)))])
    (message "Generating ptree from file listing")
    (ptree-root->ptree (cons POLLEN_TREE_ROOT_NAME files))))


;; Try loading from ptree file, or failing that, synthesize ptree.
(define/contract (make-project-ptree [project-dir PROJECT_ROOT])
  (() (directory-pathish?) . ->* . ptree?)
  (define ptree-source (build-path project-dir DEFAULT_POLLEN_TREE))
  (if (file-exists? ptree-source)
      (ptree-source->ptree ptree-source)
      (directory->ptree project-dir)))


(module+ test
  (let ([sample-main `(POLLEN_TREE_ROOT_NAME "foo" "bar" (one (two "three")))])
    (check-equal? (ptree-root->ptree sample-main) 
                  `(POLLEN_TREE_ROOT_NAME "foo" "bar" (one (two "three"))))))



;; return the parent of a given name
(define/contract (parent name [ptree current-ptree])
  (((or/c ptree-name? false?)) (ptree?) . ->* . (or/c ptree-name? false?)) 
  (and name
       (if (member (->string name) (map (λ(x) (->string (if (list? x) (car x) x))) (cdr ptree)))
           (->string (car ptree))
           (ormap (λ(x) (parent name x)) (filter list? ptree)))))


(module+ test
  (define test-ptree-main `(ptree-main "foo" "bar" (one (two "three"))))
  (define test-ptree (ptree-root->ptree test-ptree-main))
  (check-equal? (parent 'three test-ptree) "two")
  (check-equal? (parent "three" test-ptree) "two")
  (check-false (parent #f test-ptree))
  (check-false (parent 'nonexistent-name test-ptree)))


; get children of a particular name
(define/contract (children name [ptree current-ptree])
  (((or/c ptree-name? false?)) (ptree?) . ->* . (or/c (listof ptree-name?) false?))  
  (and name 
       (if (equal? (->string name) (->string (car ptree)))
           (map (λ(x) (->string (if (list? x) (car x) x))) (cdr ptree))
           (ormap (λ(x) (children name x)) (filter list? ptree)))))

(module+ test
  (check-equal? (children 'one test-ptree) (list "two"))
  (check-equal? (children 'two test-ptree) (list "three"))
  (check-false (children 'three test-ptree))
  (check-false (children #f test-ptree))
  (check-false (children 'fooburger test-ptree)))


;; find all siblings on current level: go up to parent and ask for children
(define/contract (siblings name [ptree current-ptree])
  ;; this never returns false: name is always a sibling of itself.
  ;; todo: how to use input value in contract? e.g., to check that name is part of output list
  ((ptree-name?) (ptree?) . ->* . (or/c (listof string?) false?))  
  (children (parent name ptree) ptree))


(module+ test
  (check-equal? (siblings 'one test-ptree) '("foo" "bar" "one"))
  (check-equal? (siblings 'foo test-ptree) '("foo" "bar" "one"))
  (check-equal? (siblings 'two test-ptree) '("two"))
  (check-false (siblings 'invalid-key test-ptree)))



(define/contract (siblings-split name [ptree current-ptree])
  ((ptree-name?) (ptree?) . ->* . (values (or/c (listof ptree-name?) false?) 
                                          (or/c (listof ptree-name?) false?)))
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
  ((ptree-name?) (ptree?) . ->* . (or/c ptree-name? false?))
  (let ([siblings (siblings-left name ptree)])
    (and siblings (last siblings))))

(module+ test
  (check-equal? (sibling-previous 'bar test-ptree) "foo")
  (check-false (sibling-previous 'foo test-ptree)))

;; get name immediately to the right in tree
(define/contract (sibling-next name [ptree current-ptree])
  ((ptree-name?) (ptree?) . ->* . (or/c ptree-name? false?))
  (let ([siblings (siblings-right name ptree)])
    (and siblings (first siblings))))

(module+ test
  (check-equal? (sibling-next 'foo test-ptree) "bar")
  (check-false (sibling-next 'one test-ptree)))


;; flatten tree to sequence
(define/contract (all-names [ptree current-ptree])
  (ptree? . -> . (listof string?))
  ; use cdr to get rid of root tag at front
  (map ->string (cdr (flatten ptree)))) 

(module+ test
  (check-equal? (all-names test-ptree) '("foo" "bar" "one" "two" "three")))



;; helper function for get-previous-names and get-next-names
(define/contract (adjacent-names side name [ptree current-ptree])
  ((symbol? ptree-name?) (ptree?) . ->* . (or/c (listof ptree-name?) false?))
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
(define/contract (previous* name [ptree current-ptree])
  ((ptree-name?) (ptree?) . ->* . (or/c (listof ptree-name?) false?))
  (adjacent-names 'left name ptree))

(module+ test
  (check-equal? (previous* 'one test-ptree) '("foo" "bar"))
  (check-equal? (previous* 'three test-ptree) '("foo" "bar" "one" "two"))
  (check-false (previous* 'foo test-ptree)))




;; get sequence of next names
(define (next* name [ptree current-ptree])
  ((ptree-name?) (ptree?) . ->* . (or/c (listof ptree-name?) false?))
  (adjacent-names 'right name ptree))

(module+ test
  (check-equal? (next* 'foo test-ptree) '("bar" "one" "two" "three"))
  (check-equal? (next* 'one test-ptree) '("two" "three"))
  (check-false (next* 'three test-ptree)))

;; get name immediately previous
(define/contract (previous name [ptree current-ptree])
  ((ptree-name?) (ptree?) . ->* . (or/c ptree-name? false?))
  (let ([result (previous* name ptree)])
    (and result (last result))))

(module+ test
  (check-equal? (previous 'one test-ptree) "bar")
  (check-equal? (previous 'three test-ptree) "two")
  (check-false (previous 'foo test-ptree)))

;; get name immediately next
(define (next name [ptree current-ptree])
  ((ptree-name?) (ptree?) . ->* . (or/c ptree-name? false?))
  (let ([result (next* name ptree)])
    (and result (first result))))

(module+ test
  (check-equal? (next 'foo test-ptree) "bar")
  (check-equal? (next 'one test-ptree) "two")
  (check-false (next 'three test-ptree)))





(define/contract (name->url name [files current-url-context])
  ((ptree-name?) ((listof pathish?)) . ->* . (or/c ptree-name? false?))
  ;; upconvert all files to their output path
  ;; then remove duplicates because some sources might have already been rendered
  (define output-paths (remove-duplicates (map ->output-path files) equal?))
  ;; find ones that match name
  (define matching-paths (filter (λ(x) (equal? (->string x) (->string name))) output-paths))
  
  (cond
    [((len matching-paths) . = . 1) (->string (car matching-paths))]
    [((len matching-paths) . > . 1) (error "More than one matching URL for" name)]
    [else #f] ))

(define ptree-name->url name->url)


(module+ test
  (define files '("foo.html" "bar.html" "bar.html.p" "zap.html" "zap.xml"))
  (check-equal? (name->url 'foo.html files) "foo.html")
  (check-equal? (name->url 'bar.html files) "bar.html")
  ;;  (check-equal? (name->url 'zap files) 'error) ;; todo: how to test error?
  (check-false (name->url 'hee files)))



;; this sets default input for following functions
(define/contract (ptree-root->ptree tx)
  ;; (not/c ptree) prevents ptrees from being accepted as input
  ((and/c tagged-xexpr?) . -> . ptree?)
  tx)


(module+ test
  (set! test-ptree-main `(ptree-main "foo" "bar" (one (two "three"))))
  (check-equal? (ptree-root->ptree test-ptree-main) 
                `(ptree-main "foo" "bar" (one (two "three")))))




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



(define current-ptree '())

(define/contract (set-current-ptree ptree)
  (ptree? . -> . void?)
  (set! current-ptree ptree))

(set-current-ptree '(ptree-root))

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
(set-current-url-context PROJECT_ROOT)

;; used to convert here-path into here
(define/contract (path->ptree-name path)
  (pathish? . -> . ptree-name?)
  (->string (->output-path (find-relative-path PROJECT_ROOT (->path path)))))


#|
(module+ main
  (displayln "Running module main")
  (set-current-ptree (make-project-ptree (->path "/Users/MB/git/bpt/")))
  (set-current-url-context "/Users/MB/git/bpt/")
  (ptree-previous (ptree-previous 'what-is-typography.html))
  (name->url "how-to-pay-for-this-book.html"))
|#