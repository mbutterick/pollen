#lang racket/base

(require rackunit "../file.rkt" "../world.rkt" sugar)

(check-true (sourceish? "foo.svg"))
(check-false (sourceish? "foo.gif"))

(check-true (urlish? (->path "/Users/MB/home.html")))
(check-true (urlish? "/Users/MB/home.html?foo=bar"))
(check-true (urlish? (->symbol "/Users/MB/home")))

(check-true (pathish? (->path "/Users/MB/home")))
(check-true (pathish? "/Users/MB/home"))
(check-true (pathish? (->symbol "/Users/MB/home")))

(check-true (directories-equal? "/Users/MB/foo" "/Users/MB/foo/"))
(check-false (directories-equal? "/Users/MB/foo" "Users/MB/foo"))

(check-equal? (get-enclosing-dir "/Users/MB/foo.txt") (->path "/Users/MB/"))
(check-equal? (get-enclosing-dir "/Users/MB/foo/") (->path "/Users/MB/"))

(check-true (has-binary-ext? "foo.MP3"))
(check-false (has-binary-ext? "foo.py"))

(check-true (preproc-source? "foo.pp"))
(check-false (preproc-source? "foo.bar"))
(check-false (preproc-source? #f))


(check-true (pagetree-source? (format "foo.~a" world:pagetree-source-ext)))
(check-false (pagetree-source? (format "~a.foo" world:pagetree-source-ext)))
(check-false (pagetree-source? #f))

(check-true (markup-source? "foo.pm"))
(check-false (markup-source? "foo.p"))
(check-false (markup-source? #f))

(check-true (template-source? "foo.html.pt"))
(check-false (template-source? "foo.html"))
(check-false (template-source? #f))



(check-equal? (->preproc-source-path (->path "foo.pp")) (->path "foo.pp"))
(check-equal? (->preproc-source-path (->path "foo.html")) (->path "foo.html.pp"))
(check-equal? (->preproc-source-path "foo") (->path "foo.pp"))
(check-equal? (->preproc-source-path 'foo) (->path "foo.pp"))

(check-equal? (->output-path (->path "foo.pmap")) (->path "foo.pmap"))
(check-equal? (->output-path "foo.html") (->path "foo.html"))
(check-equal? (->output-path 'foo.html.p) (->path "foo.html"))
(check-equal? (->output-path (->path "/Users/mb/git/foo.html.p")) (->path "/Users/mb/git/foo.html"))
(check-equal? (->output-path "foo.xml.p") (->path "foo.xml"))
(check-equal? (->output-path 'foo.barml.p) (->path "foo.barml"))

(check-equal? (->markup-source-path (->path "foo.pm")) (->path "foo.pm"))
(check-equal? (->markup-source-path (->path "foo.html")) (->path "foo.html.pm"))
(check-equal? (->markup-source-path "foo") (->path "foo.pm"))
(check-equal? (->markup-source-path 'foo) (->path "foo.pm"))