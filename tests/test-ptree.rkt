#lang racket/base
(require rackunit)
(require "../ptree.rkt" "../world.rkt")


(check-true (pnode? "foo-bar"))
(check-true (pnode? "Foo_Bar_0123"))
(check-true (pnode? 'foo-bar))
(check-true (pnode? "foo-bar.p"))
(check-true (pnode? "/Users/MB/foo-bar"))
(check-false (pnode? #f))
(check-false (pnode? ""))
(check-false (pnode? " "))

(check-true (ptree? '(foo)))
(check-true (ptree? '(foo (hee))))
(check-true (ptree? '(foo (hee (uncle "foo")))))


(define test-ptree-main `(ptree-main "foo" "bar" (one (two "three"))))
(define test-ptree (ptree-root->ptree test-ptree-main))
(check-equal? (parent 'three test-ptree) "two")
(check-equal? (parent "three" test-ptree) "two")
(check-false (parent #f test-ptree))
(check-false (parent 'nonexistent-name test-ptree))


(check-equal? (children 'one test-ptree) (list "two"))
(check-equal? (children 'two test-ptree) (list "three"))
(check-false (children 'three test-ptree))
(check-false (children #f test-ptree))
(check-false (children 'fooburger test-ptree))

(check-equal? (siblings 'one test-ptree) '("foo" "bar" "one"))
(check-equal? (siblings 'foo test-ptree) '("foo" "bar" "one"))
(check-equal? (siblings 'two test-ptree) '("two"))
(check-false (siblings #f test-ptree))
(check-false (siblings 'invalid-key test-ptree))

(check-equal? (left-adjacents 'one test-ptree) '("foo" "bar"))
(check-equal? (left-adjacents 'three test-ptree) '("foo" "bar" "one" "two"))
(check-false (left-adjacents 'foo test-ptree))

(check-equal? (previous 'one test-ptree) "bar")
(check-equal? (previous 'three test-ptree) "two")
(check-false (previous 'foo test-ptree))

(check-equal? (next 'foo test-ptree) "bar")
(check-equal? (next 'one test-ptree) "two")
(check-false (next 'three test-ptree))

(check-equal? (ptree->list test-ptree) '("foo" "bar" "one" "two" "three"))


(let ([sample-main `(world:pollen-tree-root-name "foo" "bar" (one (two "three")))])
  (check-equal? (ptree-root->ptree sample-main) 
                `(world:pollen-tree-root-name "foo" "bar" (one (two "three")))))

(define files '("foo.html" "bar.html" "bar.html.p" "zap.html" "zap.xml"))
(check-equal? (pnode->url/paths 'foo.html files) "foo.html")
(check-equal? (pnode->url/paths 'bar.html files) "bar.html")
;;  (check-equal? (name->url 'zap files) 'error) ;; todo: how to test error?
(check-false (pnode->url/paths 'hee files))


(set! test-ptree-main `(,world:ptree-root-node "foo" "bar" (one (two "three"))))
(check-equal? (ptree-root->ptree test-ptree-main) 
              `(,world:ptree-root-node "foo" "bar" (one (two "three"))))