#lang racket/base
(require (for-syntax racket/base racket/syntax))
(require racket/contract racket/path)
(require (only-in racket/path filename-extension))
(require "world.rkt" sugar)

;; for files like svg that are not source in pollen terms,
;; but have a textual representation separate from their display.
(define+provide/contract (sourceish? x)
  (any/c . -> . coerce/boolean?)
  (define sourceish-extensions (list "svg"))
  (try ((get-ext x) . in? . sourceish-extensions)
       (except [exn:fail? (λ(e) #f)])))


;; if something can be successfully coerced to a url,
;; it's urlish.
(define+provide/contract (urlish? x)
  (any/c . -> . coerce/boolean?)
  (try (->url x) (except [exn:fail? (λ(e) #f)])))


;; if something can be successfully coerced to a path,
;; it's pathish.
(define+provide/contract (pathish? x)
  (any/c . -> . coerce/boolean?)
  (try (->path x) (except [exn:fail? (λ(e) #f)])))


;; like pathish, but for directories
;; todo: is this contract too restrictive?
;; pathish doesn't require the path to exist,
;; but this one does.
(define+provide/contract (directory-pathish? x)
  (any/c . -> . coerce/boolean?)
  (and (pathish? x) (directory-exists? (->path x))))

;; compare directories by their exploded path elements,
;; not by equal?, which will give wrong result if no slash on the end
(define+provide/contract (directories-equal? dirx diry)
  (coerce/path? coerce/path? . -> . coerce/boolean?)
  (equal? (explode-path dirx) (explode-path diry)))

(define+provide/contract (get-enclosing-dir p)
  (coerce/path? . -> . path?)
  (simplify-path (build-path p 'up)))

;; helper function for ptree
;; make paths absolute to test whether files exist,
;; then convert back to relative
(define+provide/contract (visible? path)
  (coerce/path? . -> . coerce/boolean?)
  (not ((->string path) . starts-with? . ".")))

(define+provide/contract (visible-files dir)
  (directory-pathish? . -> . (listof path?))
  (filter visible? 
          (map (λ(p) (find-relative-path dir p)) 
               (filter file-exists? 
                       (directory-list dir #:build? #t)))))

;; does path have a certain extension
(define+provide/contract (has-ext? x ext)
  (coerce/path? coerce/symbol? . -> . coerce/boolean?)
  (define ext-of-path (filename-extension x))
  (and ext-of-path (equal? (string-downcase (bytes->string/utf-8 ext-of-path)) (string-downcase (symbol->string ext)))))


;; todo: add extensions
(define binary-extensions
  '(gif jpg jpeg mp3 png zip pdf ico tar ai eps))

(define+provide/contract (has-binary-ext? x)
  (coerce/path? . -> . coerce/boolean?)
  (ormap (λ(ext) (has-ext? x ext)) binary-extensions))


;; get file extension as a string, or return #f 
;; (consistent with filename-extension behavior)
(define+provide/contract (get-ext x)
  (coerce/path? . -> . (or/c string? #f))
  (let ([fe-result (filename-extension x)])
    (and fe-result (bytes->string/utf-8 fe-result))))


;; put extension on path
;; use local contract here because this function is used within module
(define/contract+provide (add-ext x ext)
  (coerce/string? coerce/string? . -> . coerce/path?)
  (string-append x "." ext))

;; take one extension off path
(define+provide/contract (remove-ext x)
  (coerce/path? . -> . path?)
  ;; pass through hidden files (those starting with a dot)
  (if (x . starts-with? . ".")
      x
      (path-replace-suffix x "")))


;; take all extensions off path
(define+provide/contract (remove-all-ext x)
  (coerce/path? . -> . path?)
  ;; pass through hidden files (those starting with a dot)
  (if (x . starts-with? . ".")
      x
      (let ([path-with-removed-ext (remove-ext x)])
        (if (equal? x path-with-removed-ext)
            x
            (remove-all-ext path-with-removed-ext)))))



(define-syntax (make-source-utility-functions stx)
  (syntax-case stx ()
    [(_ stem file-ext)
     (with-syntax ([stem-source? (format-id stx "~a-source?" #'stem)]
                   [has-stem-source? (format-id stx "has-~a-source?" #'stem)]
                   [has/is-stem-source? (format-id stx "has/is-~a-source?" #'stem)]
                   [->stem-source-path (format-id stx "->~a-source-path" #'stem)]
                   [->stem-source+output-paths (format-id stx "->~a-source+output-paths" #'stem)])
       #'(begin
           ;; does file have particular extension
           (define+provide/contract (stem-source? x)
             (any/c . -> . boolean?)
             (->boolean (and (pathish? x) (has-ext? (->path x) file-ext))))
           
           ;; does the source-ified version of the file exist
           (define+provide/contract (has-stem-source? x)
             (any/c . -> . boolean?)
             (->boolean (and (pathish? x) (file-exists? (->stem-source-path (->path x))))))
           
           ;; it's a file-ext source file, or a file that's the result of a file-ext source
           (define+provide/contract (has/is-stem-source? x)
             (any/c . -> . boolean?)
             (->boolean (and (pathish? x) (ormap (λ(proc) (proc (->path x))) (list stem-source? has-stem-source?)))))
           
           ;; add the file extension if it's not there
           (define+provide/contract (->stem-source-path x)
             (pathish? . -> . path?)
             (->path (if (stem-source? x) x (add-ext x file-ext))))
           
           (define+provide/contract (->stem-source+output-paths path)
             (pathish? . -> . (values path? path?))
             (values (->complete-path (->stem-source-path path))
                     (->complete-path (->output-path path))))))]))


(make-source-utility-functions preproc world:preproc-source-ext)
(make-source-utility-functions null world:null-source-ext)
(make-source-utility-functions ptree world:ptree-source-ext)
(make-source-utility-functions markup world:markup-source-ext)
(make-source-utility-functions template world:template-source-ext)



(define+provide/contract (->output-path x)
  (coerce/path? . -> . coerce/path?)
  (if (or (markup-source? x) (preproc-source? x) (null-source? x))
      (remove-ext x)
      x))


(define+provide/contract (project-files-with-ext ext)
  (coerce/symbol? . -> . (listof complete-path?))
  (map ->complete-path (filter (λ(i) (has-ext? i ext)) (directory-list (world:current-project-root)))))

;; to identify unsaved sources in DrRacket
(define (unsaved-source? path-string)
  ((substring (->string path-string) 0 7) . equal? . "unsaved"))

;; todo: write tests for project-files-with-ext
