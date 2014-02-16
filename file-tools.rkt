#lang racket/base
(require racket/contract racket/path)
(require (only-in racket/path filename-extension))
(require "world.rkt" sugar)

(module+ test (require rackunit))

;; for files like svg that are not source in pollen terms,
;; but have a textual representation separate from their display.
(define/provide/contract (sourceish? x)
  (any/c . -> . coerce/boolean?)
  (define sourceish-extensions (list "svg"))
  (try ((get-ext x) . in? . sourceish-extensions)
       (except [exn:fail? (λ(e) #f)])))


;; if something can be successfully coerced to a url,
;; it's urlish.
(define/provide/contract (urlish? x)
  (any/c . -> . coerce/boolean?)
  (try (->url x) (except [exn:fail? (λ(e) #f)])))


;; if something can be successfully coerced to a path,
;; it's pathish.
(define/provide/contract (pathish? x)
  (any/c . -> . coerce/boolean?)
  (try (->path x) (except [exn:fail? (λ(e) #f)])))


;; like pathish, but for directories
;; todo: is this contract too restrictive?
;; pathish doesn't require the path to exist,
;; but this one does.
(define/provide/contract (directory-pathish? x)
  (any/c . -> . coerce/boolean?)
  (and (pathish? x) (directory-exists? (->path x))))

;; compare directories by their exploded path elements,
;; not by equal?, which will give wrong result if no slash on the end
(define/provide/contract (directories-equal? dirx diry)
  (coerce/path? coerce/path? . -> . coerce/boolean?)
  (equal? (explode-path dirx) (explode-path diry)))

(define/provide/contract (get-enclosing-dir p)
  (coerce/path? . -> . path?)
  (simplify-path (build-path p 'up)))

;; helper function for ptree
;; make paths absolute to test whether files exist,
;; then convert back to relative
(define/provide/contract (visible? path)
  (coerce/path? . -> . coerce/boolean?)
  (not ((->string path) . starts-with? . ".")))

(define/provide/contract (visible-files dir)
  (directory-pathish? . -> . (listof path?))
  (filter visible? 
          (map (λ(p) (find-relative-path dir p)) 
               (filter file-exists? 
                       (directory-list dir #:build? #t)))))

;; does path have a certain extension
(define/provide/contract (has-ext? x ext)
  (coerce/path? coerce/symbol? . -> . coerce/boolean?)
  (define ext-of-path (filename-extension x))
  (and ext-of-path (equal? (string-downcase (bytes->string/utf-8 ext-of-path)) (string-downcase (symbol->string ext)))))


;; todo: add extensions
(define binary-extensions
  '(gif jpg jpeg mp3 png zip pdf ico tar ai eps))

(define/provide/contract (has-binary-ext? x)
  (coerce/path? . -> . coerce/boolean?)
  (ormap (λ(ext) (has-ext? x ext)) binary-extensions))


;; get file extension as a string, or return #f 
;; (consistent with filename-extension behavior)
(define/provide/contract (get-ext x)
  (coerce/path? . -> . (or/c string? #f))
  (let ([fe-result (filename-extension x)])
    (and fe-result (bytes->string/utf-8 fe-result))))


;; put extension on path
;; use local contract here because this function is used within module
(provide add-ext)
(define/contract (add-ext x ext)
  (coerce/string? coerce/string? . -> . coerce/path?)
  (string-append x "." ext))

;; take one extension off path
(define/provide/contract (remove-ext x)
  (coerce/path? . -> . path?)
  ;; pass through hidden files (those starting with a dot)
  (if (x . starts-with? . ".")
      x
      (path-replace-suffix x "")))


;; take all extensions off path
(define/provide/contract (remove-all-ext x)
  (coerce/path? . -> . path?)
  ;; pass through hidden files (those starting with a dot)
  (if (x . starts-with? . ".")
      x
      (let ([path-with-removed-ext (remove-ext x)])
        (if (equal? x path-with-removed-ext)
            x
            (remove-all-ext path-with-removed-ext)))))



;; todo: tests for these predicates

(define/provide/contract (preproc-source? x)
  (any/c . -> . coerce/boolean?)
  (and (pathish? x) (has-ext? (->path x) PREPROC_SOURCE_EXT)))


(define/provide/contract (has-preproc-source? x)
  (any/c . -> . coerce/boolean?)
  (and (pathish? x) (file-exists? (->preproc-source-path (->path x)))))

(define/provide/contract (has-decoder-source? x)
  (any/c . -> . coerce/boolean?)
  (and (pathish? x) (file-exists? (->decoder-source-path (->path x)))))

(define/provide/contract (needs-preproc? x)
  (any/c . -> . coerce/boolean?)
  ; it's a preproc source file, or a file that's the result of a preproc source
  (and (pathish? x) (ormap (λ(proc) (proc (->path x))) (list preproc-source? has-preproc-source?))))

(define/provide/contract (needs-template? x)
  (any/c . -> . coerce/boolean?)
  ; it's a pollen source file
  ; or a file (e.g., html) that has a pollen source file
  (and (pathish? x) (ormap (λ(proc) (proc (->path x))) (list decoder-source? has-decoder-source?))))


(define/provide/contract (ptree-source? x)
  (any/c . -> . coerce/boolean?)
  (and (pathish? x) ((->path x) . has-ext? . PTREE_SOURCE_EXT)))


(define/provide/contract (decoder-source? x)
  (any/c . -> . coerce/boolean?)
  (and (pathish? x) (has-ext? x DECODER_SOURCE_EXT)))


(define/provide/contract (template-source? x)
  (any/c . -> . coerce/boolean?)
  (and (pathish? x)
       (let-values ([(dir name ignore) (split-path x)])
         (equal? (get (->string name) 0) TEMPLATE_SOURCE_PREFIX))))

;; predicate for files that are eligible to be required
;; from the project/require directory
;; todo: extend this beyond just racket files?
(define/provide/contract (project-require-file? x)
  (any/c . -> . coerce/boolean?)
  (and (pathish? x) (has-ext? x 'rkt)))



;; todo: tighten these input contracts
;; so that, say, a source-path cannot be input for make-preproc-source-path
(define/provide/contract (->preproc-source-path x)
  (coerce/path? . -> . coerce/path?)
  (if (preproc-source? x)
      x
      (add-ext x PREPROC_SOURCE_EXT)))

(define/provide/contract (->output-path x)
  (coerce/path? . -> . coerce/path?)
  (if (or (decoder-source? x) (preproc-source? x))
      (remove-ext x)
      x))

;; turns input into corresponding pollen source path
;; does not, however, validate that new path exists
;; todo: should it? I don't think so, sometimes handy to make the name for later use
;; OK to use pollen source as input (comes out the same way)
(define/provide/contract (->decoder-source-path x)
  (coerce/path? . -> . coerce/path?)
  (if (decoder-source? x)
      x
      (add-ext x DECODER_SOURCE_EXT)))


(define/provide/contract (project-files-with-ext ext)
  (coerce/symbol? . -> . (listof complete-path?))
  (map ->complete-path (filter (λ(i) (has-ext? i ext)) (directory-list PROJECT_ROOT))))

;; to identify unsaved sources in DrRacket
(define (unsaved-source? path-string)
  ((substring (->string path-string) 0 7) . equal? . "unsaved"))

;; todo: write tests for project-files-with-ext
