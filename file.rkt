#lang racket/base
(require (for-syntax racket/base racket/syntax))
(require racket/contract racket/path racket/match)
(require (only-in racket/path filename-extension))
(require "world.rkt" sugar/define sugar/file sugar/string sugar/coerce sugar/test)

;; for files like svg that are not source in pollen terms,
;; but have a textual representation separate from their display.
(define+provide/contract (sourceish? x)
  (any/c . -> . coerce/boolean?)
  (define sourceish-extensions (list "svg"))
  (with-handlers ([exn:fail? (λ(e) #f)])
    (member (get-ext x) sourceish-extensions)))

(module-test-external
 (check-true (sourceish? "foo.svg"))
 (check-false (sourceish? "foo.gif")))


;; compare directories by their exploded path elements,
;; not by equal?, which will give wrong result if no slash on the end
(define+provide/contract (directories-equal? dirx diry)
  (coerce/path? coerce/path? . -> . coerce/boolean?)
  (equal? (explode-path dirx) (explode-path diry)))

(module-test-external
 (check-true (directories-equal? "/Users/MB/foo" "/Users/MB/foo/"))
 (check-false (directories-equal? "/Users/MB/foo" "Users/MB/foo")))


;; helper function for pagetree
;; make paths absolute to test whether files exist,
;; then convert back to relative
(define+provide/contract (visible? path)
  (coerce/path? . -> . coerce/boolean?)
  (not ((->string path) . starts-with? . ".")))

(define (paths? x) (and (list? x) (andmap path? x)))
(define (complete-paths? x) (and (list? x) (andmap complete-path? x)))

(define+provide/contract (visible-files dir)
  (pathish? . -> . paths?)
  (filter visible? 
          (map (λ(p) (find-relative-path dir p)) 
               (filter file-exists? 
                       (directory-list dir #:build? #t)))))


(define+provide/contract (escape-last-ext x [escape-char (world:current-extension-escape-char)])
  ((pathish?) (char?) . ->* . coerce/path?)
  ;; if x has a file extension, reattach it with the escape char
  (define current-ext (get-ext x))
  (if current-ext
      (format "~a~a~a" (->string (remove-ext x)) escape-char current-ext)
      x))

(module-test-external
 (require sugar/coerce)
 (check-equal? (escape-last-ext "foo") (->path "foo"))
 (check-equal? (escape-last-ext "foo.html") (->path "foo_html"))
 (check-equal? (escape-last-ext "foo.html" #\$) (->path "foo$html")))


(define+provide/contract (unescape-ext x [escape-char (world:current-extension-escape-char)])
  ((pathish?) (char?) . ->* . coerce/path?)
  ;; if x has an escaped extension, unescape it.
  (define xstr (->string x))
  (define pat (regexp (format "(.*)[~a](.*)$" escape-char)))
  (define results (regexp-match pat xstr))
  (if results
      (match-let ([(list _ filename ext) results])
        (add-ext filename ext))
      x))


(module-test-external
 (require sugar/coerce)
 (check-equal? (unescape-ext "foo") (->path "foo"))
 (check-equal? (unescape-ext "foo.html") (->path "foo.html"))
 (check-equal? (unescape-ext "foo_html") (->path "foo.html"))
 (check-equal? (unescape-ext "foo$html" #\$) (->path "foo.html"))
 (check-equal? (unescape-ext "foo_bar__html") (->path "foo_bar_.html"))
 (check-equal? (unescape-ext "foo$bar$$html" #\$) (->path "foo$bar$.html")))


(define-syntax (make-source-utility-functions stx)
  (syntax-case stx ()
    [(_ stem)
     (let ([stem-datum (syntax->datum #'stem)])
       (with-syntax ([world:current-stem-source-ext (format-id stx "world:current-~a-source-ext" #'stem)]
                     [stem-source? (format-id stx "~a-source?" #'stem)]
                     [get-stem-source (format-id stx "get-~a-source" #'stem)]
                     [has-stem-source? (format-id stx "has-~a-source?" #'stem)]
                     [has/is-stem-source? (format-id stx "has/is-~a-source?" #'stem)]
                     [->stem-source-path (format-id stx "->~a-source-path" #'stem)]
                     [->stem-source-paths (format-id stx "->~a-source-paths" #'stem)]
                     [->stem-source+output-paths (format-id stx "->~a-source+output-paths" #'stem)])
         #`(begin
             ;; does file have particular extension
             (define+provide (stem-source? x)
               (->boolean (and (pathish? x) (has-ext? (->path x) (world:current-stem-source-ext)))))
             
             ;; non-theoretical: want the first possible source that exists in the filesystem
             (define+provide (get-stem-source x)
               (and (pathish? x) 
                    (let ([source-paths (->stem-source-paths (->path x))])
                      (and source-paths (ormap (λ(sp) (and (file-exists? sp) sp)) source-paths)))))
             
             ;; does the source-ified version of the file exist
             (define+provide (has-stem-source? x)
               (->boolean (get-stem-source x)))
             
             ;; it's a file-ext source file, or a file that's the result of a file-ext source
             (define+provide (has/is-stem-source? x)
               (->boolean (and (pathish? x) (ormap (λ(proc) (proc (->path x))) (list stem-source? has-stem-source?)))))
             
             ;; get first possible source path (does not check filesystem)
             (define+provide/contract (->stem-source-path x)
               (pathish? . -> . (or/c #f path?))
               (define paths (->stem-source-paths x))
               (and paths (car paths)))
             
             ;; get all possible source paths (does not check filesystem)
             (define+provide/contract (->stem-source-paths x)
               (pathish? . -> . (or/c #f (non-empty-listof path?)))
               (define results (if (stem-source? x) 
                                   (list x) ; already has the source extension
                                   #,(if (eq? stem-datum 'scribble)
                                         #'(if (x . has-ext? . 'html) ; different logic for scribble sources
                                               (list (add-ext (remove-ext* x) (world:current-stem-source-ext)))
                                               #f)
                                         #'(list* (add-ext x (world:current-stem-source-ext))
                                                  (if (get-ext x)
                                                      (list (add-ext (escape-last-ext x) (world:current-stem-source-ext)))
                                                      null)))))
               (and results (map ->path results)))
             
             ;; coerce either a source or output file to both
             (define+provide/contract (->stem-source+output-paths path)
               (pathish? . -> . (values path? path?))
               ;; get the real source path if available, otherwise a theoretical path
               (values (->complete-path (or (get-stem-source path) (->stem-source-path path)))
                       (->complete-path (->output-path path)))))))]))

(make-source-utility-functions preproc)

(define+provide/contract (->source+output-paths source-or-output-path)
  (complete-path? . -> . (values complete-path? complete-path?))
  ;; file-proc returns two values, but ormap only wants one
  (define file-proc (ormap (λ(test file-proc) (and (test source-or-output-path) file-proc))
                           (list has/is-null-source? has/is-preproc-source? has/is-markup-source? has/is-scribble-source? has/is-markdown-source? has/is-template-source?)
                           (list ->null-source+output-paths ->preproc-source+output-paths ->markup-source+output-paths ->scribble-source+output-paths ->markdown-source+output-paths ->template-source+output-paths)))
  (file-proc source-or-output-path))



(module-test-external
 (require sugar/coerce)
 (check-true (preproc-source? "foo.pp"))
 (check-false (preproc-source? "foo.bar"))
 (check-false (preproc-source? #f))
 (check-equal? (->preproc-source-paths (->path "foo.pp")) (list (->path "foo.pp")))
 (check-equal? (->preproc-source-paths (->path "foo.html")) (list (->path "foo.html.pp") (->path "foo_html.pp")))
 (check-equal? (->preproc-source-paths "foo") (list (->path "foo.pp")))
 (check-equal? (->preproc-source-paths 'foo) (list (->path "foo.pp")))
 (check-equal? (->preproc-source-path (->path "foo.pp")) (->path "foo.pp"))
 (check-equal? (->preproc-source-path (->path "foo.html")) (->path "foo.html.pp"))
 (check-equal? (->preproc-source-path "foo") (->path "foo.pp"))
 (check-equal? (->preproc-source-path 'foo) (->path "foo.pp")))

(make-source-utility-functions null)

(make-source-utility-functions pagetree)
(module-test-external
 (require pollen/world)
 (check-true (pagetree-source? (format "foo.~a" (world:current-pagetree-source-ext))))
 (check-false (pagetree-source? (format "~a.foo" (world:current-pagetree-source-ext))))
 (check-false (pagetree-source? #f)))

(make-source-utility-functions markup)
(module-test-external
 (require sugar/coerce)
 (check-true (markup-source? "foo.pm"))
 (check-false (markup-source? "foo.p"))
 (check-false (markup-source? #f))
 (check-equal? (->markup-source-paths (->path "foo.pm")) (list (->path "foo.pm")))
 (check-equal? (->markup-source-paths (->path "foo.html")) (list (->path "foo.html.pm") (->path "foo_html.pm")))
 (check-equal? (->markup-source-paths "foo") (list (->path "foo.pm")))
 (check-equal? (->markup-source-paths 'foo) (list (->path "foo.pm")))
 (check-equal? (->markup-source-path (->path "foo.pm")) (->path "foo.pm"))
 (check-equal? (->markup-source-path (->path "foo.html")) (->path "foo.html.pm"))
 (check-equal? (->markup-source-path "foo") (->path "foo.pm"))
 (check-equal? (->markup-source-path 'foo) (->path "foo.pm")))

(make-source-utility-functions markdown)

(make-source-utility-functions template)
(module-test-external
 (check-true (template-source? "foo.html.pt"))
 (check-false (template-source? "foo.html"))
 (check-false (template-source? #f)))

(make-source-utility-functions scribble)


(define/contract+provide (->source-path path)
  (coerce/path? . -> . (or/c #f path?))
  (ormap (λ(proc) (proc path)) (list get-markup-source get-markdown-source get-preproc-source get-null-source get-scribble-source)))


(define+provide/contract (->output-path x)
  (coerce/path? . -> . coerce/path?)
  (cond
    [(or (markup-source? x) (preproc-source? x) (null-source? x) (markdown-source? x) (template-source? x)) (unescape-ext (remove-ext x))]
    [(scribble-source? x) (add-ext (remove-ext x) 'html)]
    [else x]))

(module-test-external
 (require sugar/coerce)
 (check-equal? (->output-path (->path "foo.pmap")) (->path "foo.pmap"))
 (check-equal? (->output-path "foo.html") (->path "foo.html"))
 (check-equal? (->output-path 'foo.html.p) (->path "foo.html"))
 (check-equal? (->output-path (->path "/Users/mb/git/foo.html.p")) (->path "/Users/mb/git/foo.html"))
 (check-equal? (->output-path "foo.xml.p") (->path "foo.xml"))
 (check-equal? (->output-path 'foo.barml.p) (->path "foo.barml"))
 
 (check-equal? (->output-path 'foo_html.p) (->path "foo.html"))
 (check-equal? (->output-path (->path "/Users/mb/git/foo_html.p")) (->path "/Users/mb/git/foo.html"))
 (check-equal? (->output-path "foo_xml.p") (->path "foo.xml"))
 (check-equal? (->output-path 'foo_barml.p) (->path "foo.barml")))

(define+provide/contract (project-files-with-ext ext)
  (coerce/symbol? . -> . complete-paths?)
  (map ->complete-path (filter (λ(i) (has-ext? i ext)) (directory-list (world:current-project-root)))))


(define+provide (racket-source? x)
  (->boolean (and (pathish? x) (has-ext? (->path x) 'rkt))))


;; to identify unsaved sources in DrRacket
(define (unsaved-source? path-string)
  ((substring (->string path-string) 0 7) . equal? . "unsaved"))


(define+provide (magic-directory? path)
  (and (directory-exists? path) 
       (or (ends-with? (path->string path) "compiled"))))


(define+provide (cache-file? path)
  (ormap (λ(cache-name) (ends-with? (path->string path) cache-name)) world:cache-names))


(define+provide (pollen-related-file? file)
  (ormap (λ(proc) (proc file)) (list
                                preproc-source? 
                                markup-source?
                                markdown-source?
                                template-source?
                                pagetree-source?
                                scribble-source?
                                null-source?
                                racket-source?
                                magic-directory?
                                cache-file?
                                (world:current-unpublished-path?))))