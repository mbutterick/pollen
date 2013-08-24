#lang racket/base
(require racket/contract)
(require (only-in racket/path filename-extension))
(require "world.rkt" "readability.rkt")

(provide (all-defined-out))

(module+ test (require rackunit))


; helper functions for regenerate functions
(define pollen-file-root (current-directory))

;; does path have a certain extension
(define/contract (has-ext? path ext)
  (path? symbol? . -> . boolean?)
  (define ext-of-path (filename-extension path))
  (and ext-of-path (equal? (bytes->string/utf-8 ext-of-path) (->string ext))))

(module+ test
  (define foo-path-strings '("foo" "foo.txt" "foo.bar" "foo.bar.txt"))
  (define-values (foo-path foo.txt-path foo.bar-path foo.bar.txt-path) 
    (apply values (map string->path foo-path-strings)))
  ;; test the sample paths before using them for other tests
  (define foo-paths (list foo-path foo.txt-path foo.bar-path foo.bar.txt-path))
  (for-each check-equal? (map path->string foo-paths) foo-path-strings))


(module+ test
  (check-false (has-ext? foo-path 'txt)) 
  (check-true (has-ext? foo.txt-path 'txt))
  (check-true (has-ext? foo.bar.txt-path 'txt))
  (check-false (has-ext? foo.bar.txt-path 'doc))) ; wrong extension


;; get file extension as a string
(define/contract (get-ext path)
  (path? . -> . string?)
  (bytes->string/utf-8 (filename-extension path)))

(module+ test
  (check-equal? (get-ext (->path "foo.txt")) "txt")
  ;; todo: how should get-ext handle input that has no extension?
  ;(check-equal? (get-ext (->path "foo")) "")
  )


;; put extension on path
(define/contract (add-ext path ext)
  (path? (or/c symbol? string?) . -> . path?)
  (string->path (string-append (->string path) "." (->string ext))))

(module+ test
  (check-equal? (add-ext (string->path "foo") "txt") (string->path "foo.txt")))

;; take one extension off path
(define/contract (remove-ext path)
  (path? . -> . path?)
  (path-replace-suffix path ""))

(module+ test  
  (check-equal? (remove-ext foo-path) foo-path)
  (check-equal? (remove-ext foo.txt-path) foo-path)
  (check-equal? (remove-ext foo.bar.txt-path) foo.bar-path)
  (check-not-equal? (remove-ext foo.bar.txt-path) foo-path)) ; does not remove all extensions


;; take all extensions off path
(define/contract (remove-all-ext path)
  (path? . -> . path?)
  (define path-with-removed-ext (remove-ext path))
  (if (equal? path path-with-removed-ext)
      path
      (remove-all-ext path-with-removed-ext)))

(module+ test  
  (check-equal? (remove-all-ext foo-path) foo-path)
  (check-equal? (remove-all-ext foo.txt-path) foo-path)
  (check-not-equal? (remove-all-ext foo.bar.txt-path) foo.bar-path) ; removes more than one ext
  (check-equal? (remove-all-ext foo.bar.txt-path) foo-path))

;; superfluous: use file-name-from-path in racket/path

#|(define/contract (filename-of path)
  (complete-path? . -> . path?)
  (define-values (dir filename ignored) (split-path path))
  filename)

(module+ test 
 (check-equal? (filename-of (build-path (current-directory) "pollen-file-tools.rkt")) (->path "pollen-file-tools.rkt")))|#


(define/contract (preproc-source? x)
  (any/c . -> . boolean?)
  (has-ext? (->path x) POLLEN_PREPROC_EXT))

(define/contract (has-preproc-source? x)
  (any/c . -> . boolean?)
  (file-exists? (make-preproc-source-path (->path x))))

(define/contract (has-pollen-source? x)
  (any/c . -> . boolean?)
  (file-exists? (make-pollen-source-path (->path x))))

(define/contract (needs-preproc? x)
  (any/c . -> . boolean?)
  ; it's a preproc source file, or a file that's the result of a preproc source
  (ormap (λ(proc) (proc (->path x))) (list preproc-source? has-preproc-source?)))

(define/contract (needs-template? x)
  (any/c . -> . boolean?)
  ; it's a pollen source file
  ; or a file (e.g., html) that has a pollen source file
  (ormap (λ(proc) (proc (->path x))) (list pollen-source? has-pollen-source?)))

(define/contract (pmap-source? x)
  (any/c . -> . boolean?)
  (has-ext? (->path x) POLLEN_MAP_EXT))

(define/contract (pollen-source? x)
  (any/c . -> . boolean?)
  (has-ext? (->path x) POLLEN_SOURCE_EXT))

;; this is for regenerate module.
;; when we want to be friendly with inputs for functions that require a path.
;; Strings & symbols often result from xexpr parsing
;; and are trivially converted to paths.
;; so let's say close enough.
(define/contract (pathish? x)
  (any/c . -> . boolean?)
  (->boolean (or path? string? symbol?)))


;; todo: tighten these input contracts
;; so that, say, a source-path cannot be input for make-preproc-source-path
(define/contract (make-preproc-source-path path)
  (path? . -> . path?)
  (add-ext path POLLEN_PREPROC_EXT))

(define/contract (make-preproc-output-path path)
  (path? . -> . path?)
  (remove-ext path))

(define/contract (make-pollen-output-path thing ext)
  (pathish? (or/c string? symbol?) . -> . path?)
  (add-ext (remove-ext (->path thing)) ext))

(module+ test
  (check-equal? (make-pollen-output-path (->path "foo.p") 'html) (->path "foo.html"))
  (check-equal? (make-pollen-output-path (->path "/Users/mb/git/foo.p") 'html) (->path "/Users/mb/git/foo.html"))
  (check-equal? (make-pollen-output-path "foo" 'xml) (->path "foo.xml"))
  (check-equal? (make-pollen-output-path 'foo 'barml) (->path "foo.barml")))

;; turns input into corresponding pollen source path
;; does not, however, validate that new path exists
;; todo: should it? I don't think so, sometimes handy to make the name for later use
;; OK to use pollen source as input (comes out the same way)
(define/contract (make-pollen-source-path thing)
  (pathish? . -> . path?)
  (add-ext (remove-ext (->path thing)) POLLEN_SOURCE_EXT))

(module+ test
  (check-equal? (make-pollen-source-path (->path "foo.p")) (->path "foo.p"))
  (check-equal? (make-pollen-source-path "foo") (->path "foo.p"))
  (check-equal? (make-pollen-source-path 'foo) (->path "foo.p")))

(define/contract (project-files-with-ext ext)
  (symbol? . -> . (listof complete-path?))
  (map ->complete-path (filter (λ(i) (has-ext? i ext)) (directory-list pollen-file-root))))

;; todo: write tests for project-files-with-ext
