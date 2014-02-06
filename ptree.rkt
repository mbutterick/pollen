#lang racket/base
(require racket/contract racket/match xml/path racket/bool)
(require "tools.rkt" "world.rkt" "debug.rkt" "decode.rkt")

(module+ test (require rackunit))

(provide ptree-node? ptree? parent children previous next)

(define/contract (ptree-node? x)
  (any/c . -> . boolean?)
  (and (stringish? x) (not (whitespace? (->string x)))))

(define/contract (ptree-node?/error x)
  (any/c . -> . boolean?)
  (or (ptree-node? x) (error "Not a valid ptree node:" x)))


(module+ test
  (check-true (ptree-node? "foo-bar"))
  (check-true (ptree-node? "Foo_Bar_0123"))
  (check-true (ptree-node? 'foo-bar))
  (check-true (ptree-node? "foo-bar.p"))
  (check-true (ptree-node? "/Users/MB/foo-bar"))
  (check-false (ptree-node? #f))
  (check-false (ptree-node? ""))
  (check-false (ptree-node? " ")))


(define/contract (ptree? x)
  (any/c . -> . boolean?)
  (and (tagged-xexpr? x) (andmap (λ(i) (or (ptree-node? i) (ptree? i))) x)))

(module+ test
  (check-true (ptree? '(foo)))
  (check-true (ptree? '(foo (hee))))
  (check-true (ptree? '(foo (hee (uncle "foo"))))))


(define/contract (file->ptree path)
  (pathish? . -> . ptree?)
  (message "Loading ptree file" (file-name-from-path path))
  (dynamic-require path POLLEN_ROOT))

(define/contract (directory->ptree dir)
  (directory-pathish? . -> . ptree?)
  (let ([files (map remove-ext (filter (λ(x) (has-ext? x POLLEN_DECODER_EXT)) (directory-list dir)))])
    (message "Generating ptree from file listing of" dir)
    (ptree-root->ptree (cons POLLEN_TREE_ROOT_NAME files))))


;; Try loading from ptree file, or failing that, synthesize ptree.
(define/contract (make-project-ptree [project-dir PROJECT_ROOT])
  (() (directory-pathish?) . ->* . ptree?)
  (define ptree-source (build-path project-dir DEFAULT_POLLEN_TREE))
  (if (file-exists? ptree-source)
      (file->ptree ptree-source)
      (directory->ptree project-dir)))


(module+ test
  (let ([sample-main `(POLLEN_TREE_ROOT_NAME "foo" "bar" (one (two "three")))])
    (check-equal? (ptree-root->ptree sample-main) 
                  `(POLLEN_TREE_ROOT_NAME "foo" "bar" (one (two "three"))))))


(define/contract (parent node [ptree (current-ptree)])
  (((or/c ptree-node? false?)) (ptree?) . ->* . (or/c ptree-node? false?)) 
  (and node
       (if (member (->string node) (map (λ(x) (->string (if (list? x) (car x) x))) (cdr ptree)))
           (->string (car ptree))
           (ormap (λ(x) (parent node x)) (filter list? ptree)))))


(module+ test
  (define test-ptree-main `(ptree-main "foo" "bar" (one (two "three"))))
  (define test-ptree (ptree-root->ptree test-ptree-main))
  (check-equal? (parent 'three test-ptree) "two")
  (check-equal? (parent "three" test-ptree) "two")
  (check-false (parent #f test-ptree))
  (check-false (parent 'nonexistent-name test-ptree)))


(define/contract (children node [ptree (current-ptree)])
  (((or/c ptree-node? false?)) (ptree?) . ->* . (or/c (listof ptree-node?) false?))  
  (and node 
       (if (equal? (->string node) (->string (car ptree)))
           (map (λ(x) (->string (if (list? x) (car x) x))) (cdr ptree))
           (ormap (λ(x) (children node x)) (filter list? ptree)))))

(module+ test
  (check-equal? (children 'one test-ptree) (list "two"))
  (check-equal? (children 'two test-ptree) (list "three"))
  (check-false (children 'three test-ptree))
  (check-false (children #f test-ptree))
  (check-false (children 'fooburger test-ptree)))


(define/contract (siblings node [ptree (current-ptree)])
  (((or/c ptree-node? false?)) (ptree?) . ->* . (or/c (listof string?) false?))  
  (children (parent node ptree) ptree))


(module+ test
  (check-equal? (siblings 'one test-ptree) '("foo" "bar" "one"))
  (check-equal? (siblings 'foo test-ptree) '("foo" "bar" "one"))
  (check-equal? (siblings 'two test-ptree) '("two"))
  (check-false (siblings #f test-ptree))
  (check-false (siblings 'invalid-key test-ptree)))



;; flatten tree to sequence
(define/contract (ptree->list [ptree (current-ptree)])
  (ptree? . -> . (listof string?))
  ; use cdr to get rid of root tag at front
  (map ->string (cdr (flatten ptree)))) 

(module+ test
  (check-equal? (ptree->list test-ptree) '("foo" "bar" "one" "two" "three")))



(define/contract (adjacents side node [ptree (current-ptree)])
  ((symbol? (or/c ptree-node? false?)) (ptree?) . ->* . (or/c (listof ptree-node?) false?))
  (and node
       (let ([result ((if (equal? side 'left) 
                          takef
                          takef-right) (ptree->list ptree) 
                                       (λ(y) (not (equal? (->string node) (->string y)))))])
         (and (not (empty? result)) result))))

(module+ test
  (check-equal? (adjacents 'left 'one test-ptree) '("foo" "bar"))
  (check-equal? (adjacents 'left 'three test-ptree) '("foo" "bar" "one" "two"))
  (check-false (adjacents 'left 'foo test-ptree)))

(define (left-adjacents node [ptree (current-ptree)]) (adjacents 'left node ptree))
(define (right-adjacents node [ptree (current-ptree)]) (adjacents 'right node ptree))

(define/contract (previous node [ptree (current-ptree)])
  (((or/c ptree-node? false?)) (ptree?) . ->* . (or/c ptree-node? false?))
  (let ([result (left-adjacents node ptree)])
    (and result (last result))))

(module+ test
  (check-equal? (previous 'one test-ptree) "bar")
  (check-equal? (previous 'three test-ptree) "two")
  (check-false (previous 'foo test-ptree)))


(define (next node [ptree (current-ptree)])
  (((or/c ptree-node? false?)) (ptree?) . ->* . (or/c ptree-node? false?))
  (let ([result (right-adjacents node ptree)])
    (and result (first result))))

(module+ test
  (check-equal? (next 'foo test-ptree) "bar")
  (check-equal? (next 'one test-ptree) "two")
  (check-false (next 'three test-ptree)))



(define/contract (name->url name [files current-url-context])
  ((ptree-node?) ((listof pathish?)) . ->* . (or/c ptree-node? false?))
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
  (andmap ptree-node?/error (filter-not whitespace? (flatten x))))

;; contract for ptree-source-decode
(define/contract (unique-names? x)
  (any/c . -> . boolean?)
  ;; use map ->string to make keys comparable
  (elements-unique? #:loud #t (map ->string (filter-not whitespace? (flatten x)))))


(define/contract (ptree-source-decode . elements)
  (() #:rest (and/c valid-names? unique-names?) . ->* . ptree?)
  (ptree-root->ptree (decode (cons POLLEN_TREE_ROOT_NAME elements)
                             #:xexpr-elements-proc (λ(xs) (filter-not whitespace? xs)))))


(define current-ptree (make-parameter `(,POLLEN_TREE_ROOT_NAME)))
(define current-url-context (make-parameter PROJECT_ROOT))


;; used to convert here-path into here
(define/contract (path->ptree-node path)
  (pathish? . -> . ptree-node?)
  (->string (->output-path (find-relative-path PROJECT_ROOT (->path path)))))


#|
(module+ main
  (displayln "Running module main")
  (set-current-ptree (make-project-ptree (->path "/Users/MB/git/bpt/")))
  (set-current-url-context "/Users/MB/git/bpt/")
  (ptree-previous (ptree-previous 'what-is-typography.html))
  (name->url "how-to-pay-for-this-book.html"))
|#