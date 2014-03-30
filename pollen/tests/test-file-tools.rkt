#lang racket/base

(module+ test (require rackunit "../file.rkt" "../world.rkt" sugar))

(module+ test
  (check-true (sourceish? "foo.svg"))
  (check-false (sourceish? "foo.gif")))

(module+ test
  (check-true (urlish? (->path "/Users/MB/home.html")))
  (check-true (urlish? "/Users/MB/home.html?foo=bar"))
  (check-true (urlish? (->symbol "/Users/MB/home"))))

(module+ test
  (check-true (pathish? (->path "/Users/MB/home")))
  (check-true (pathish? "/Users/MB/home"))
  (check-true (pathish? (->symbol "/Users/MB/home"))))

(module+ test
  (check-true (directories-equal? "/Users/MB/foo" "/Users/MB/foo/"))
  (check-false (directories-equal? "/Users/MB/foo" "Users/MB/foo")))

(module+ test
  (check-equal? (get-enclosing-dir "/Users/MB/foo.txt") (->path "/Users/MB/"))
  (check-equal? (get-enclosing-dir "/Users/MB/foo/") (->path "/Users/MB/")))

(module+ test
  (check-true (has-binary-ext? "foo.MP3"))
  (check-false (has-binary-ext? "foo.py")))


(module+ test
  (define foo-path-strings '("foo" "foo.txt" "foo.bar" "foo.bar.txt"))
  (define-values (foo-path foo.txt-path foo.bar-path foo.bar.txt-path) 
    (apply values (map ->path foo-path-strings)))
  ;; test the sample paths before using them for other tests
  (define foo-paths (list foo-path foo.txt-path foo.bar-path foo.bar.txt-path))
  (for-each check-equal? (map ->string foo-paths) foo-path-strings))

(module+ test
  (check-false (has-ext? foo-path 'txt)) 
  (check-true (foo.txt-path . has-ext? . 'txt))
  (check-true ((->path "foo.TXT") . has-ext? . 'txt))
  (check-true (has-ext? foo.bar.txt-path 'txt))
  (check-false (foo.bar.txt-path . has-ext? . 'doc))) ; wrong extension


(module+ test
  (check-equal? (get-ext (->path "foo.txt")) "txt")
  (check-false (get-ext "foo")))

(module+ test
  (check-equal? (add-ext (string->path "foo") "txt") (string->path "foo.txt")))
(module+ test  
  (check-equal? (remove-ext foo-path) foo-path)
  (check-equal? (remove-ext (->path ".foo.txt")) (->path ".foo.txt"))
  (check-equal? (remove-ext foo.txt-path) foo-path)
  (check-equal? (remove-ext foo.bar.txt-path) foo.bar-path)
  (check-not-equal? (remove-ext foo.bar.txt-path) foo-path)) ; does not remove all extensions


(module+ test  
  (check-equal? (remove-all-ext foo-path) foo-path)
  (check-equal? (remove-all-ext foo.txt-path) foo-path)
  (check-equal? (remove-all-ext (->path ".foo.txt")) (->path ".foo.txt"))
  (check-not-equal? (remove-all-ext foo.bar.txt-path) foo.bar-path) ; removes more than one ext
  (check-equal? (remove-all-ext foo.bar.txt-path) foo-path))
(module+ test
  (check-true (preproc-source? "foo.p"))
  (check-false (preproc-source? "foo.bar"))
  (check-false (preproc-source? #f)))

(module+ test
  (check-true (pagetree-source? (format "foo.~a" world:pagetree-source-ext)))
  (check-false (pagetree-source? (format "~a.foo" world:pagetree-source-ext)))
  (check-false (pagetree-source? #f)))
(module+ test
  (check-true (markup-source? "foo.pm"))
  (check-false (markup-source? "foo.p"))
  (check-false (markup-source? #f)))

(module+ test
  (check-true (template-source? "foo.html.pt"))
  (check-false (template-source? "foo.html"))
  (check-false (template-source? #f)))


(module+ test
  (check-equal? (->preproc-source-path (->path "foo.p")) (->path "foo.p"))
  (check-equal? (->preproc-source-path (->path "foo.html")) (->path "foo.html.p"))
  (check-equal? (->preproc-source-path "foo") (->path "foo.p"))
  (check-equal? (->preproc-source-path 'foo) (->path "foo.p")))
(module+ test
  (check-equal? (->output-path (->path "foo.pmap")) (->path "foo.pmap"))
  (check-equal? (->output-path "foo.html") (->path "foo.html"))
  (check-equal? (->output-path 'foo.html.p) (->path "foo.html"))
  (check-equal? (->output-path (->path "/Users/mb/git/foo.html.p")) (->path "/Users/mb/git/foo.html"))
  (check-equal? (->output-path "foo.xml.p") (->path "foo.xml"))
  (check-equal? (->output-path 'foo.barml.p) (->path "foo.barml")))
(module+ test
  (check-equal? (->markup-source-path (->path "foo.pm")) (->path "foo.pm"))
  (check-equal? (->markup-source-path (->path "foo.html")) (->path "foo.html.pm"))
  (check-equal? (->markup-source-path "foo") (->path "foo.pm"))
  (check-equal? (->markup-source-path 'foo) (->path "foo.pm")))