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
  (with-handlers ([exn:fail? (λ(e) #f)])
    ((get-ext x) . in? . sourceish-extensions)))


;; compare directories by their exploded path elements,
;; not by equal?, which will give wrong result if no slash on the end
(define+provide/contract (directories-equal? dirx diry)
  (coerce/path? coerce/path? . -> . coerce/boolean?)
  (equal? (explode-path dirx) (explode-path diry)))



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


(define-syntax (make-source-utility-functions stx)
  (syntax-case stx ()
    [(_ stem)
     (let ([stem-datum (syntax->datum #'stem)])
       (with-syntax ([file-ext (format-id stx "world:~a-source-ext" #'stem)]
                     [stem-source? (format-id stx "~a-source?" #'stem)]
                     [get-stem-source (format-id stx "get-~a-source" #'stem)]
                     [has-stem-source? (format-id stx "has-~a-source?" #'stem)]
                     [has/is-stem-source? (format-id stx "has/is-~a-source?" #'stem)]
                     [->stem-source-path (format-id stx "->~a-source-path" #'stem)]
                     [->stem-source+output-paths (format-id stx "->~a-source+output-paths" #'stem)])
         #`(begin
             ;; does file have particular extension
             (define+provide (stem-source? x)
               (->boolean (and (pathish? x) (has-ext? (->path x) file-ext))))
             
             (define+provide (get-stem-source x)
               (and (pathish? x) 
                    (let ([source-path (->stem-source-path (->path x))])
                      (and source-path (file-exists? source-path) source-path))))
             
             ;; does the source-ified version of the file exist
             (define+provide (has-stem-source? x)
               (->boolean (get-stem-source x)))
             
             ;; it's a file-ext source file, or a file that's the result of a file-ext source
             (define+provide (has/is-stem-source? x)
               (->boolean (and (pathish? x) (ormap (λ(proc) (proc (->path x))) (list stem-source? has-stem-source?)))))
             
             ;; add the file extension if it's not there
             (define+provide/contract (->stem-source-path x)
               (pathish? . -> . (or/c #f path?))
               (define result (if (stem-source? x) 
                                  x 
                                  #,(if (equal? stem-datum 'scribble)
                                        #'(if (x . has-ext? . 'html) ; different logic for scribble sources
                                              (add-ext (remove-ext* x) file-ext)
                                              #f)
                                        #'(add-ext x file-ext))))
               (and result (->path result)))
             
             ;; coerce either a source or output file to both
             (define+provide/contract (->stem-source+output-paths path)
               (pathish? . -> . (values path? path?))
               (values (->complete-path (->stem-source-path path))
                       (->complete-path (->output-path path)))))))]))


(make-source-utility-functions preproc)
(make-source-utility-functions null)
(make-source-utility-functions pagetree)
(make-source-utility-functions markup)
(make-source-utility-functions markdown)
(make-source-utility-functions template)
(make-source-utility-functions scribble)


(define/contract+provide (->source-path path)
  (coerce/path? . -> . (or/c #f path?))
  (ormap (λ(proc) (proc path)) (list get-markup-source get-markdown-source get-preproc-source get-null-source get-scribble-source)))


(define+provide/contract (->output-path x)
  (coerce/path? . -> . coerce/path?)
  (cond
    [(or (markup-source? x) (preproc-source? x) (null-source? x) (markdown-source? x) (template-source? x)) (remove-ext x)]
    [(scribble-source? x) (add-ext (remove-ext x) 'html)]
    [else x]))


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
                                magic-directory?)))