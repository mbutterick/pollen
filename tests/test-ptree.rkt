#lang racket/base
(require rackunit)
(require "../pagetree.rkt" "../world.rkt")


(check-false (pagenode? "foo-bar"))
(check-false (pagenode? "Foo_Bar_0123"))
(check-true (pagenode? 'foo-bar))
(check-false (pagenode? "foo-bar.p"))
(check-false (pagenode? "/Users/MB/foo-bar"))
(check-false (pagenode? #f))
(check-false (pagenode? ""))
(check-false (pagenode? " "))

(check-true (pagetree? '(foo)))
(check-true (pagetree? '(foo (hee))))
(check-true (pagetree? '(foo (hee (uncle foo)))))
(check-false (pagetree? '(foo (hee hee (uncle foo)))))


(define test-pagetree-main `(pagetree-main foo bar (one (two three))))
(define test-pagetree (pagetree-root->pagetree test-pagetree-main))
(check-equal? (parent 'three test-pagetree) 'two)
(check-equal? (parent "three" test-pagetree) 'two)
(check-false (parent #f test-pagetree))
(check-false (parent 'nonexistent-name test-pagetree))


(check-equal? (children 'one test-pagetree) '(two))
(check-equal? (children 'two test-pagetree) '(three))
(check-false (children 'three test-pagetree))
(check-false (children #f test-pagetree))
(check-false (children 'fooburger test-pagetree))

(check-equal? (siblings 'one test-pagetree) '(foo bar one))
(check-equal? (siblings 'foo test-pagetree) '(foo bar one))
(check-equal? (siblings 'two test-pagetree) '(two))
(check-false (siblings #f test-pagetree))
(check-false (siblings 'invalid-key test-pagetree))

(check-equal? (previous* 'one test-pagetree) '(foo bar))
(check-equal? (previous* 'three test-pagetree) '(foo bar one two))
(check-false (previous* 'foo test-pagetree))

(check-equal? (previous 'one test-pagetree) 'bar)
(check-equal? (previous 'three test-pagetree) 'two)
(check-false (previous 'foo test-pagetree))

(check-equal? (next 'foo test-pagetree) 'bar)
(check-equal? (next 'one test-pagetree) 'two)
(check-false (next 'three test-pagetree))

(check-equal? (pagetree->list test-pagetree) '(foo bar one two three))


(let ([sample-main `(world:pollen-tree-root-name foo bar (one (two three)))])
  (check-equal? (pagetree-root->pagetree sample-main) 
                `(world:pollen-tree-root-name foo bar (one (two three)))))

(define files '("foo.html" "bar.html" "bar.html.p" "zap.html" "zap.xml"))
(check-equal? (pagenode->url/paths 'foo.html files) "foo.html")
(check-equal? (pagenode->url/paths 'bar.html files) "bar.html")
;;  (check-equal? (name->url 'zap files) 'error) ;; todo: how to test error?
(check-false (pagenode->url/paths 'hee files))


(set! test-pagetree-main `(,world:pagetree-root-node foo bar (one (two three))))
(check-equal? (pagetree-root->pagetree test-pagetree-main) 
              `(,world:pagetree-root-node foo bar (one (two three))))