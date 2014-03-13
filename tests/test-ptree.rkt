#lang racket/base
(require rackunit)
(require "../pagemap.rkt" "../world.rkt")


(check-false (node? "foo-bar"))
(check-false (node? "Foo_Bar_0123"))
(check-true (node? 'foo-bar))
(check-false (node? "foo-bar.p"))
(check-false (node? "/Users/MB/foo-bar"))
(check-false (node? #f))
(check-false (node? ""))
(check-false (node? " "))

(check-true (pagemap? '(foo)))
(check-true (pagemap? '(foo (hee))))
(check-true (pagemap? '(foo (hee (uncle foo)))))
(check-false (pagemap? '(foo (hee hee (uncle foo)))))


(define test-pagemap-main `(pagemap-main foo bar (one (two three))))
(define test-pagemap (pagemap-root->pagemap test-pagemap-main))
(check-equal? (parent 'three test-pagemap) 'two)
(check-equal? (parent "three" test-pagemap) 'two)
(check-false (parent #f test-pagemap))
(check-false (parent 'nonexistent-name test-pagemap))


(check-equal? (children 'one test-pagemap) '(two))
(check-equal? (children 'two test-pagemap) '(three))
(check-false (children 'three test-pagemap))
(check-false (children #f test-pagemap))
(check-false (children 'fooburger test-pagemap))

(check-equal? (siblings 'one test-pagemap) '(foo bar one))
(check-equal? (siblings 'foo test-pagemap) '(foo bar one))
(check-equal? (siblings 'two test-pagemap) '(two))
(check-false (siblings #f test-pagemapap))
(check-false (siblings 'invalid-key test-pagemap))

(check-equal? (previous* 'one test-pagemap) '(foo bar))
(check-equal? (previous* 'three test-pagemap) '(foo bar one two))
(check-false (previous* 'foo test-pagemap))

(check-equal? (previous 'one test-pagemap) 'bar)
(check-equal? (previous 'three test-pagemap) 'two)
(check-false (previous 'foo test-pagemap))

(check-equal? (next 'foo test-pagemap) 'bar)
(check-equal? (next 'one test-pagemap) 'two)
(check-false (next 'three test-pagemap))

(check-equal? (pagemap->list test-pagemap) '(foo bar one two three))


(let ([sample-main `(world:pollen-tree-root-name foo bar (one (two three)))])
  (check-equal? (pagemap-root->pagemap sample-main) 
                `(world:pollen-tree-root-name foo bar (one (two three)))))

(define files '("foo.html" "bar.html" "bar.html.p" "zap.html" "zap.xml"))
(check-equal? (node->url/paths 'foo.html files) "foo.html")
(check-equal? (node->url/paths 'bar.html files) "bar.html")
;;  (check-equal? (name->url 'zap files) 'error) ;; todo: how to test error?
(check-false (node->url/paths 'hee files))


(set! test-pagemap-main `(,world:pagemap-root-node foo bar (one (two three))))
(check-equal? (pagemap-root->pagemap test-pagemap-main) 
              `(,world:pagemap-root-node foo bar (one (two three))))