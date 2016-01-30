#lang racket/base
(require racket/file racket/path compiler/cm)
(require sugar/test sugar/define sugar/file sugar/coerce)
(require "private/file-utils.rkt"
         "cache.rkt"
         "private/debug.rkt"
         "private/project.rkt"
         "private/cache-utils.rkt"
         "pagetree.rkt"
         "template.rkt"
         "core.rkt"
         "private/rerequire.rkt"
         "setup.rkt")

;; used to track renders according to modification dates of component files
(define mod-date-hash (make-hash))

;; when you want to generate everything fresh. 
;; render functions will always go when no mod-date is found.
(define (reset-mod-date-hash)
  (set! mod-date-hash (make-hash)))


(module-test-internal
 (require racket/runtime-path)
 (define-runtime-path sample-dir "test/data/samples")
 (define samples (parameterize ([current-directory sample-dir])
                   (map path->complete-path (filter (λ(name) (regexp-match "sample-" name)) (directory-list ".")))))
 (define-values (sample-01 sample-02 sample-03) (apply values samples)))



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

(define+provide/contract (render* . xs)
  (() #:rest list-of-pathish? . ->* . void?)
  ;; Why not just (map render ...)?
  ;; Because certain files will pass through multiple times (e.g., templates)
  ;; And with render, they would be rendered repeatedly.
  ;; Using reset-modification-dates is sort of like session control.
  (reset-mod-date-hash) 
  (for-each (λ(x) ((if (pagetree-source? x)
                       render-pagenodes
                       render-from-source-or-output-path) x)) xs))


(define+provide/contract (render-pagenodes pagetree-or-path)
  ((or/c pagetree? pathish?) . -> . void?)
  (define pagetree (if (pagetree? pagetree-or-path)
                       pagetree-or-path
                       (cached-doc pagetree-or-path)))
  (parameterize ([current-directory (setup:current-project-root)])
    (for-each render-from-source-or-output-path (map ->complete-path (pagetree->list pagetree)))))


(define+provide/contract (render-from-source-or-output-path so-pathish)
  (pathish? . -> . void?)
  (let ([so-path (->complete-path so-pathish)])  ; so-path = source or output path (could be either) 
    (cond
      [(ormap (λ(test) (test so-path)) (list has/is-null-source? has/is-preproc-source? has/is-markup-source? has/is-scribble-source? has/is-markdown-source?)) 
       (let-values ([(source-path output-path) (->source+output-paths so-path)])
         (render-to-file-if-needed source-path #f output-path))]
      [(pagetree-source? so-path) (render-pagenodes so-path)]))
  (void))

(define (validate-output-path op caller)
  (unless op
    (raise-argument-error caller "valid output path" op)))


(define+provide/contract (render-to-file-if-needed source-path [maybe-template-path #f] [maybe-output-path #f])
  ((complete-path?) ((or/c #f complete-path?) (or/c #f complete-path?)) . ->* . void?)
  (define output-path (or maybe-output-path (->output-path source-path)))
  (validate-output-path output-path 'render-to-file-if-needed)
  (define template-path (or maybe-template-path (get-template-for source-path output-path)))
  (define render-needed?
    (cond
      [(not (file-exists? output-path)) 'file-missing]
      [(mod-date-missing-or-changed? source-path template-path) 'mod-key-missing-or-changed]
      [(not (setup:render-cache-active source-path)) 'render-cache-deactivated]
      [else #f]))
  (when render-needed?
    (render-to-file source-path template-path output-path)))


(define+provide/contract (render-to-file source-path [maybe-template-path #f] [maybe-output-path #f])
  ((complete-path?) ((or/c #f complete-path?) (or/c #f complete-path?)) . ->* . void?)
  (define output-path (or maybe-output-path (->output-path source-path)))
  (validate-output-path output-path 'render-to-file)
  (define template-path (or maybe-template-path (get-template-for source-path output-path)))
  (define render-result (render source-path template-path output-path)) ; will either be string or bytes
  (display-to-file render-result output-path #:exists 'replace
                   #:mode (if (string? render-result) 'text 'binary)))


(define+provide/contract (render source-path [maybe-template-path #f] [maybe-output-path #f])
  ((complete-path?) ((or/c #f complete-path?) (or/c #f complete-path?)) . ->* . (or/c string? bytes?))
  (define render-proc 
    (cond
      [(ormap (λ(test render-proc) (and (test source-path) render-proc))
              (list has/is-null-source? has/is-preproc-source? has/is-markup-source? has/is-scribble-source? has/is-markdown-source?)
              (list render-null-source render-preproc-source render-markup-or-markdown-source render-scribble-source render-markup-or-markdown-source))] 
      [else (error (format "render: no rendering function found for ~a" source-path))]))
  
  (define output-path (or maybe-output-path (->output-path source-path)))
  (define template-path (or maybe-template-path (get-template-for source-path output-path)))
  (message (format "rendering: /~a as /~a" (find-relative-path (setup:current-project-root) source-path)
                   (find-relative-path (setup:current-project-root) output-path)))
  (define render-result (parameterize ([setup:current-poly-target (->symbol (get-ext output-path))])
                          (apply render-proc (list source-path template-path output-path))))
  ;; wait till last possible moment to store mod dates, because render-proc may also trigger its own subrenders
  ;; e.g., of a template. 
  (update-mod-date-hash source-path template-path) 
  render-result)


(define (render-null-source source-path . ignored-paths)
  ;((complete-path?) #:rest any/c . ->* . bytes?)
  ;; All this does is copy the source. Hence, "null".
  ;; todo: add test to avoid copying if unnecessary (good idea in case the file is large)
  (file->bytes source-path))


(define (render-scribble-source source-path . ignored-paths)
  ;((complete-path?) #:rest any/c . ->* . string?)
  (define source-dir (dirname source-path))
  (dynamic-rerequire source-path) ; to suppress namespace caching by dynamic-require below
  (define scribble-render (dynamic-require 'scribble/render 'render))
  (time (parameterize ([current-directory (->complete-path source-dir)])
          ;; if there's a compiled zo file for the Scribble file,
          ;; (as is usually the case in existing packages)
          ;; it will foul up the render
          ;; so recompile first to avoid "can't redefine a constant" errors.
          (managed-compile-zo source-path)
          ;; scribble/lp files have their doc export in a 'doc submodule, so check both locations
          (define doc (dynamic-require source-path 'doc
                                       (λ _ (dynamic-require `(submod ,source-path doc) 'doc
                                                             (λ _ #f)))))
          ;; BTW this next action has side effects: scribble will copy in its core files if they don't exist.
          (when doc
            (scribble-render (list doc) (list source-path)))))
  (define result (file->string (->output-path source-path)))
  (delete-file (->output-path source-path)) ; because render promises the data, not the side effect
  result)


(define (render-preproc-source source-path . ignored-paths)
  ;((complete-path?) #:rest any/c . ->* . (or/c string? bytes?))
  (define source-dir (dirname source-path))
  (time (parameterize ([current-directory (->complete-path source-dir)])
          (render-through-eval `(begin (require pollen/cache)
                                       (cached-doc ,source-path))))))


(define (render-markup-or-markdown-source source-path [maybe-template-path #f] [maybe-output-path #f]) 
  ;((complete-path?) ((or/c #f complete-path?)(or/c #f complete-path?)) . ->* . (or/c string? bytes?))
  (define source-dir (dirname source-path))
  (define output-path (or maybe-output-path (->output-path source-path)))
  (define template-path (or maybe-template-path (get-template-for source-path output-path)))
  (unless template-path
    (raise-result-error 'render-markup-or-markdown-source "valid template path" template-path))
  (render-from-source-or-output-path template-path) ; because template might have its own preprocessor source
  (define expr-to-eval 
    `(begin 
       (require (for-syntax racket/base))
       (require pollen/private/include-template pollen/cache pollen/private/debug pollen/pagetree pollen/core)
       ,(require-directory-require-files source-path)
       (parameterize ([current-pagetree (make-project-pagetree ,(setup:current-project-root))])
         (let ([,(setup:main-export source-path) (cached-doc ,(path->string source-path))]
               [,(setup:meta-export source-path) (cached-metas ,(path->string source-path))]
               [,(setup:splicing-tag source-path) (λ xs xs)]) ; splice behavior is different in textual context
           (local-require pollen/template pollen/top)
           (define here (path->pagenode
                         (or (select-from-metas ',(setup:here-path-key source-path) ,(setup:meta-export source-path)) 'unknown)))
           (cond 
             [(bytes? ,(setup:main-export source-path)) ,(setup:main-export source-path)] ; if main export is binary, just pass it through
             [else
              ;; `include-template` is the slowest part of the operation (the eval itself is cheap)
              (include-template #:command-char ,(setup:command-char source-path) (file ,(->string (find-relative-path source-dir template-path))))])))))
  (time (parameterize ([current-directory (->complete-path source-dir)]) ; because include-template wants to work relative to source location
          (render-through-eval expr-to-eval))))


(define (templated-source? path)
  ;(complete-path? . -> . boolean?)
  (or (markup-source? path) (markdown-source? path)))


(define identity (λ(x) x))
(define+provide/contract (get-template-for source-path [maybe-output-path #f])
  ((complete-path?)((or/c #f complete-path?)) . ->* . (or/c #f complete-path?))
  
  (define (file-exists-or-has-source? p) ; p could be #f
    (and p (ormap (λ(proc) (file-exists? (proc p))) (list identity ->preproc-source-path ->null-source-path)) p))
  
  (define (get-template)
    (define source-dir (dirname source-path))
    (define output-path (or maybe-output-path (->output-path source-path)))
    (define output-path-ext (get-ext output-path))
    (define (get-template-from-metas)
      (with-handlers ([exn:fail:contract? (λ _ #f)]) ; in case source-path doesn't work with cached-require
        (parameterize ([current-directory (setup:current-project-root)])
          (let* ([source-metas (cached-metas source-path)]
                 [template-name-or-names (select-from-metas (setup:template-meta-key source-path) source-metas)] ; #f or atom or list
                 [template-name (cond
                                  [(list? template-name-or-names)
                                   (define result
                                     (memf (λ(tn) (eq? (get-ext tn) output-path-ext)) template-name-or-names)) ; #f or list
                                   (and result (car result))]
                                  [else template-name-or-names])])
            (and template-name (build-path source-dir template-name))))))
    
    (define (get-default-template)
      (and output-path-ext
           (let ([default-template-filename (add-ext (setup:template-prefix source-path) output-path-ext)])
             (find-upward-from source-path default-template-filename file-exists-or-has-source?))))

    (define (get-fallback-template)
      (and output-path-ext
           (build-path (setup:current-server-extras-path)
                       (add-ext (setup:fallback-template-prefix source-path) output-path-ext))))
    
    (or (file-exists-or-has-source? (get-template-from-metas))
        (file-exists-or-has-source? (get-default-template))
        (file-exists-or-has-source? (get-fallback-template))))   
  
  (and (templated-source? source-path) (get-template)))


(module-test-external
 (require pollen/setup sugar/file sugar/coerce)
 (define fallback.html (build-path (setup:current-server-extras-path)
                                   (add-ext (setup:fallback-template-prefix) 'html)))
 (check-equal? (get-template-for (->complete-path "foo.poly.pm")) fallback.html)
 (check-equal? (get-template-for (->complete-path "foo.html.pm")) fallback.html)
 
 (define fallback.svg (build-path (setup:current-server-extras-path)
                                  (add-ext (setup:fallback-template-prefix) 'svg)))
 (parameterize ([setup:current-poly-target 'svg])
   (check-equal? (get-template-for (->complete-path "foo.poly.pm")) fallback.svg)
   (check-equal? (get-template-for (->complete-path "foo.html.pm")) fallback.html))
 
 (define fallback.missing (build-path (setup:current-server-extras-path)
                                      (add-ext (setup:fallback-template-prefix) 'missing)))
 (parameterize ([setup:current-poly-target 'missing])
   (check-false (get-template-for (->complete-path "foo.poly.pm")))
   (check-equal? (get-template-for (->complete-path "foo.html.pm")) fallback.html)))

(define-namespace-anchor render-module-ns)
(define (render-through-eval expr-to-eval)
  ;(list? . -> . (or/c string? bytes?))
  (parameterize ([current-namespace (make-base-namespace)]
                 [current-output-port (current-error-port)])
    (namespace-attach-module (namespace-anchor->namespace render-module-ns) 'pollen/setup) ; brings in params
    (eval expr-to-eval)))