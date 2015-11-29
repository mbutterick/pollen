#lang racket/base
(require racket/file racket/path compiler/cm)
(require sugar/test sugar/define sugar/file)
(require "file.rkt" "cache.rkt" "debug.rkt" "pagetree.rkt" "project.rkt" "template.rkt" "rerequire.rkt" "world.rkt")

;; used to track renders according to modification dates of component files
(define mod-date-hash (make-hash))

;; when you want to generate everything fresh. 
;; render functions will always go when no mod-date is found.
(define (reset-mod-date-hash)
  (set! mod-date-hash (make-hash)))

(module-test-internal
 (check-pred hash? mod-date-hash))

;; using internal contracts to provide some extra safety (negligible performance hit)

(define/contract (valid-path-arg? x)
  (any/c . -> . boolean?)
  (or (equal? #f x) (complete-path? x)))

(define/contract (valid-path-args? x)
  (any/c . -> . boolean?)
  (and (list? x) (andmap valid-path-arg? x)))



(module-test-internal
 (require racket/runtime-path)
 (define-runtime-path sample-dir "test/data/samples")
 (define samples (parameterize ([current-directory sample-dir])
                   (map path->complete-path (filter (λ(name) (regexp-match "sample-" name)) (directory-list ".")))))
 (define-values (sample-01 sample-02 sample-03) (apply values samples)))


(define/contract (path->mod-date-value path)
  ((or/c #f complete-path?) . -> . (or/c #f integer?))
  (and path (file-exists? path) (file-or-directory-modify-seconds path)))

(module-test-internal
 (check-false (path->mod-date-value (path->complete-path "garbage-path.zzz")))
 (check-equal? (path->mod-date-value sample-01) (file-or-directory-modify-seconds sample-01)))


;; each key for mod-date-hash is a list of file / mod-date pairs (using pollen/cache keymaking function)
;; when a file is rendered, a new key is stored in the hash (with trivial value #t)
;; after that, the hash-key-comparision routine intrinsic to hash lookup
;; can be used to test whether a render is obsolete.
;; create a new key with current files. If the key is in the hash, the render has happened.
;; if not, a new render is needed.
(define (update-mod-date-hash source-path template-path)
  (hash-set! mod-date-hash (paths->key source-path template-path) #t))

(define (mod-date-missing-or-changed? source-path template-path)
  (not (hash-has-key? mod-date-hash (paths->key source-path template-path))))


(define (list-of-pathish? x) (and (list? x) (andmap pathish? x)))

(define/contract+provide (render-batch . xs)
  (() #:rest list-of-pathish? . ->* . void?)
  ;; Why not just (map render ...)?
  ;; Because certain files will pass through multiple times (e.g., templates)
  ;; And with render, they would be rendered repeatedly.
  ;; Using reset-modification-dates is sort of like session control.
  (reset-mod-date-hash) 
  (for-each (λ(x) ((if (pagetree-source? x)
                       render-pagetree
                       render-from-source-or-output-path) x)) xs))


(define/contract+provide (render-pagetree pagetree-or-path)
  ((or/c pagetree? pathish?) . -> . void?)
  (define pagetree (if (pagetree? pagetree-or-path)
                       pagetree-or-path
                       (cached-require pagetree-or-path (world:current-main-export))))
  (parameterize ([current-directory (world:current-project-root)])
    (for-each render-from-source-or-output-path (map ->complete-path (pagetree->list pagetree)))))


(define/contract+provide (render-from-source-or-output-path so-pathish)
  (pathish? . -> . void?)
  (let ([so-path (->complete-path so-pathish)])  ; so-path = source or output path (could be either) 
    (cond
      [(ormap (λ(test) (test so-path)) (list has/is-null-source? has/is-preproc-source? has/is-markup-source? has/is-scribble-source? has/is-markdown-source? has/is-template-source?)) 
       (let-values ([(source-path output-path) (->source+output-paths so-path)])
         (render-to-file-if-needed source-path #f output-path))]
      [(pagetree-source? so-path) (render-pagetree so-path)]))
  (void))


(define/contract (render-needed? source-path template-path output-path)
  (complete-path? (or/c #f complete-path?) complete-path? . -> . (or/c #f symbol?))
  ;; return symbol rather than boolean for extra debugging information
  (cond
    [(not (file-exists? output-path)) 'file-missing]
    [(mod-date-missing-or-changed? source-path template-path) 'mod-key-missing-or-changed]
    [(not (world:current-render-cache-active source-path)) 'render-cache-deactivated]
    [else #f]))


(define/contract+provide (render-to-file-if-needed source-path [maybe-template-path #f] [maybe-output-path #f])
  ((complete-path?) ((or/c #f complete-path?) (or/c #f complete-path?)) . ->* . void?)  
  (define output-path (or maybe-output-path (->output-path source-path)))
  (define template-path (or maybe-template-path (get-template-for source-path output-path)))
  (when (render-needed? source-path template-path output-path)
    (render-to-file source-path template-path output-path)))


(define/contract+provide (render-to-file source-path [maybe-template-path #f] [maybe-output-path #f])
  ((complete-path?) ((or/c #f complete-path?) (or/c #f complete-path?)) . ->* . void?)
  (define output-path (or maybe-output-path (->output-path source-path)))
  (define template-path (or maybe-template-path (get-template-for source-path output-path)))
  (define render-result (render source-path template-path output-path)) ; will either be string or bytes
  (display-to-file render-result output-path #:exists 'replace
                   #:mode (if (string? render-result) 'text 'binary)))


(define/contract+provide (render source-path [maybe-template-path #f] [maybe-output-path #f])
  ((complete-path?) ((or/c #f complete-path?) (or/c #f complete-path?)) . ->* . (or/c string? bytes?))
  (define render-proc 
    (cond
      [(ormap (λ(test render-proc) (and (test source-path) render-proc))
              (list has/is-null-source? has/is-preproc-source? has/is-markup-source? has/is-scribble-source? has/is-markdown-source? has/is-template-source?)
              (list render-null-source render-preproc-source render-markup-or-markdown-source render-scribble-source render-markup-or-markdown-source render-preproc-source))] 
      [else (error (format "render: no rendering function found for ~a" source-path))]))
  
  (define output-path (or maybe-output-path (->output-path source-path)))
  (define template-path (or maybe-template-path (get-template-for source-path output-path)))
  (message (format "rendering: /~a as /~a" (find-relative-path (world:current-project-root) source-path)
                   (find-relative-path (world:current-project-root) output-path)))
  (define render-result (parameterize ([world:current-poly-target (->symbol (get-ext output-path))])
                          (apply render-proc (list source-path template-path output-path))))
  ;; wait till last possible moment to store mod dates, because render-proc may also trigger its own subrenders
  ;; e.g., of a template. 
  (update-mod-date-hash source-path template-path) 
  render-result)


(define/contract (render-null-source source-path . ignored-paths)
  ((complete-path?) #:rest any/c . ->* . bytes?)
  ;; All this does is copy the source. Hence, "null".
  ;; todo: add test to avoid copying if unnecessary (good idea in case the file is large)
  (file->bytes source-path))


(define/contract (render-scribble-source source-path . ignored-paths)
  ((complete-path?) #:rest any/c . ->* . string?)
  (define source-dir (dirname source-path))
  (dynamic-rerequire source-path) ; to suppress namespace caching by dynamic-require below
  (define scribble-render (dynamic-require 'scribble/render 'render))
  (time (parameterize ([current-directory (->complete-path source-dir)])
          ;; if there's a compiled zo file for the Scribble file,
          ;; (as is usually the case in existing packages)
          ;; it will foul up the render
          ;; so recompile first to avoid "can't redefine a constant" errors.
          (managed-compile-zo source-path)
          ;; BTW this next action has side effects: scribble will copy in its core files if they don't exist.
          (scribble-render (list (dynamic-require source-path 'doc)) (list source-path))))
  (define result (file->string (->output-path source-path)))
  (delete-file (->output-path source-path)) ; because render promises the data, not the side effect
  result)


(define/contract (render-preproc-source source-path . ignored-paths)
  ((complete-path?) #:rest any/c . ->* . (or/c string? bytes?))
  (define source-dir (dirname source-path))
  (time (parameterize ([current-directory (->complete-path source-dir)])
          (render-through-eval `(begin (require pollen/cache)
                                       (cached-require ,source-path ',(world:current-main-export)))))))


(define/contract (render-markup-or-markdown-source source-path [maybe-template-path #f] [maybe-output-path #f]) 
  ((complete-path?) ((or/c #f complete-path?)(or/c #f complete-path?)) . ->* . (or/c string? bytes?))
  (define source-dir (dirname source-path))
  (define output-path (or maybe-output-path (->output-path source-path)))
  (define template-path (or maybe-template-path (get-template-for source-path output-path)))
  (when (not template-path)
    (raise-result-error 'render-markup-or-markdown-source "valid template path" template-path))
  (render-from-source-or-output-path template-path) ; because template might have its own preprocessor source
  (define expr-to-eval 
    `(begin 
       (require (for-syntax racket/base))
       (require pollen/include-template pollen/cache pollen/debug pollen/pagetree)
       ,(require-directory-require-files source-path)
       (parameterize ([current-pagetree (make-project-pagetree ,(world:current-project-root))])
         (let ([,(world:current-main-export) (cached-require ,(path->string source-path) ',(world:current-main-export))]
               [,(world:current-meta-export) (cached-require ,(path->string source-path) ',(world:current-meta-export))])
           (local-require pollen/template pollen/top)
           (define here (metas->here ,(world:current-meta-export)))
           (cond 
             [(bytes? ,(world:current-main-export)) ,(world:current-main-export)] ; if main export is binary, just pass it through
             [else
              ;; `include-template` is the slowest part of the operation (the eval itself is cheap)
              (include-template #:command-char ,(world:current-command-char) (file ,(->string (find-relative-path source-dir template-path))))])))))
  (time (parameterize ([current-directory (->complete-path source-dir)]) ; because include-template wants to work relative to source location
          (render-through-eval expr-to-eval))))


(define/contract (templated-source? path)
  (complete-path? . -> . boolean?)
  (or (markup-source? path) (markdown-source? path)))


(define identity (λ(x) x))
(define/contract+provide (get-template-for source-path [maybe-output-path #f])
  ((complete-path?)((or/c #f complete-path?)) . ->* . (or/c #f complete-path?))
  
  (define (file-exists-or-has-source? p) ; p could be #f
    (and p (ormap (λ(proc) (file-exists? (proc p))) (list identity ->template-source-path ->preproc-source-path ->null-source-path)) p))
  
  (define (get-template)
    (define source-dir (dirname source-path))
    (define output-path (or maybe-output-path (->output-path source-path)))
    (define output-path-ext (get-ext output-path))
    (define (get-template-from-metas)
      (with-handlers ([exn:fail:contract? (λ _ #f)]) ; in case source-path doesn't work with cached-require
        (parameterize ([current-directory (world:current-project-root)])
          (let* ([source-metas (cached-require source-path (world:current-meta-export))]
                 [template-name-or-names (select-from-metas (world:current-template-meta-key) source-metas)] ; #f or atom or list
                 [template-name (cond
                                  [(list? template-name-or-names)
                                   (define result
                                     (memf (λ(tn) (equal? (get-ext tn) output-path-ext)) template-name-or-names)) ; #f or list
                                   (and result (car result))]
                                  [else template-name-or-names])])
            (and template-name (build-path source-dir template-name))))))
    
    (define (get-default-template)
      (and output-path-ext
           (let ([default-template-filename (add-ext (world:current-default-template-prefix) output-path-ext)])
             (find-upward-from source-path default-template-filename file-exists-or-has-source?))))
    
    (define (get-fallback-template)
      (and output-path-ext
           (build-path (world:current-server-extras-path)
                       (add-ext (world:current-fallback-template-prefix) output-path-ext))))
    
    (or (file-exists-or-has-source? (get-template-from-metas))
        (file-exists-or-has-source? (get-default-template))
        (file-exists-or-has-source? (get-fallback-template))))   
  
  (and (templated-source? source-path) (get-template)))


(module-test-external
 (require pollen/world sugar/file sugar/coerce)
 (define fallback.html (build-path (world:current-server-extras-path)
                                   (add-ext (world:current-fallback-template-prefix) 'html)))
 (check-equal? (get-template-for (->complete-path "foo.poly.pm")) fallback.html)
 (check-equal? (get-template-for (->complete-path "foo.html.pm")) fallback.html)
 
 (define fallback.svg (build-path (world:current-server-extras-path)
                                  (add-ext (world:current-fallback-template-prefix) 'svg)))
 (parameterize ([world:current-poly-target 'svg])
   (check-equal? (get-template-for (->complete-path "foo.poly.pm")) fallback.svg)
   (check-equal? (get-template-for (->complete-path "foo.html.pm")) fallback.html))
 
 (define fallback.missing (build-path (world:current-server-extras-path)
                                      (add-ext (world:current-fallback-template-prefix) 'missing)))
 (parameterize ([world:current-poly-target 'missing])
   (check-false (get-template-for (->complete-path "foo.poly.pm")))
   (check-equal? (get-template-for (->complete-path "foo.html.pm")) fallback.html)))

(define-namespace-anchor anchor-to-this-namespace)
(define/contract (render-through-eval expr-to-eval)
  (list? . -> . (or/c string? bytes?))
  (parameterize ([current-namespace (make-base-namespace)]
                 [current-output-port (current-error-port)])
    (namespace-attach-module (namespace-anchor->namespace anchor-to-this-namespace) 'pollen/world) ; brings in params
    (eval expr-to-eval)))