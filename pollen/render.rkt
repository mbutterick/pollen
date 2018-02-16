#lang racket/base
(require racket/file
         racket/path
         compiler/cm
         sugar/test
         sugar/define
         sugar/file
         sugar/coerce
         "private/file-utils.rkt"
         "cache.rkt"
         "private/debug.rkt"
         "private/project.rkt"
         "private/cache-utils.rkt"
         "pagetree.rkt"
         "template.rkt"
         "core.rkt"
         "setup.rkt")

;; used to track renders according to modification dates of component files
(define mod-date-hash (make-hash))

;; when you want to generate everything fresh. 
;; render functions will always go when no mod-date is found.
(define (reset-mod-date-hash!) (set! mod-date-hash (make-hash)))


(module-test-internal
 (require racket/runtime-path)
 (define-runtime-path sample-dir "test/data/samples")
 (define samples (parameterize ([current-directory sample-dir])
                   (map path->complete-path (filter (λ (name) (regexp-match "sample-" name)) (directory-list ".")))))
 (define-values (sample-01 sample-02 sample-03) (apply values samples)))



;; each key for mod-date-hash is a list of file / mod-date pairs (using pollen/cache keymaking function)
;; when a file is rendered, a new key is stored in the hash (with trivial value #t)
;; after that, the hash-key-comparision routine intrinsic to hash lookup
;; can be used to test whether a render is obsolete.
;; create a new key with current files. If the key is in the hash, the render has happened.
;; if not, a new render is needed.
(define (update-mod-date-hash! source-path template-path)
  (hash-set! mod-date-hash (paths->key source-path template-path) #t))

(define (mod-date-missing-or-changed? source-path template-path)
  (not (hash-has-key? mod-date-hash (paths->key source-path template-path))))


(define (list-of-pathish? x) (and (list? x) (andmap pathish? x)))


(define+provide/contract (render-batch . xs)
  (() #:rest list-of-pathish? . ->* . void?)
  ;; Why not just (for-each render ...)?
  ;; Because certain files will pass through multiple times (e.g., templates)
  ;; And with render, they would be rendered repeatedly.
  ;; Using reset-modification-dates is sort of like session control.
  (reset-mod-date-hash!) 
  (for-each render-from-source-or-output-path xs))


(define+provide/contract (render-pagenodes pagetree-or-path)
  ((or/c pagetree? pathish?) . -> . void?)
  (define pagetree (if (pagetree? pagetree-or-path)
                       pagetree-or-path
                       (cached-doc pagetree-or-path)))
  (parameterize ([current-directory (current-project-root)])
    (apply render-batch (map ->complete-path (pagetree->list pagetree)))))


(define+provide/contract (render-from-source-or-output-path so-pathish)
  (pathish? . -> . void?)
  (define so-path (->complete-path so-pathish)) ; so-path = source or output path (could be either) 
  (cond
    [(for/or ([pred (in-list (list has/is-null-source?
                                   has/is-preproc-source?
                                   has/is-markup-source?
                                   has/is-scribble-source?
                                   has/is-markdown-source?))])
       (pred so-path))
     (define-values (source-path output-path) (->source+output-paths so-path))
     (render-to-file-if-needed source-path #f output-path)]
    [(pagetree-source? so-path) (render-pagenodes so-path)])
  (void))

(define render-ram-cache (make-hash))

;; note that output and template order is reversed from typical
(define (render-to-file-base caller
                             force?
                             source-path
                             maybe-output-path
                             maybe-template-path)
  (define output-path (or maybe-output-path (->output-path source-path)))
  (unless output-path
    (raise-argument-error caller "valid output path" output-path))
  (define template-path (or maybe-template-path (get-template-for source-path output-path)))
  (define render-needed?
    (cond
      [force?]
      [(not (file-exists? output-path)) 'file-missing]
      [(mod-date-missing-or-changed? source-path template-path) 'mod-key-missing-or-changed]
      [(not (setup:render-cache-active source-path)) 'render-cache-deactivated]
      [else #f]))
  (when render-needed?
    (define render-result
      (let ([key (paths->key source-path template-path output-path)])
        (hash-ref! render-ram-cache
                   ;; within a session, this will prevent repeat players like "template.html.p"
                   ;; from hitting the file cache repeatedly
                   key
                   (λ () 
                     (cache-ref! key
                                 (λ () (render source-path template-path output-path))
                                 #:dest-path 'output
                                 #:notify-cache-use
                                 (λ (str)
                                   (message (format "rendering: /~a (from cache)"
                                                    (find-relative-path (current-project-root) output-path))))))))) ; will either be string or bytes
    (display-to-file render-result output-path
                     #:exists 'replace
                     #:mode (if (string? render-result) 'text 'binary))))


(define+provide/contract (render-to-file-if-needed source-path [maybe-template-path #f] [maybe-output-path #f])
  ((complete-path?) ((or/c #f complete-path?) (or/c #f complete-path?)) . ->* . void?)
  (render-to-file-base 'render-to-file-if-needed #f source-path maybe-output-path maybe-template-path))


(define+provide/contract (render-to-file source-path [maybe-template-path #f] [maybe-output-path #f])
  ((complete-path?) ((or/c #f complete-path?) (or/c #f complete-path?)) . ->* . void?)
  (render-to-file-base 'render-to-file #t source-path maybe-output-path maybe-template-path))


(define+provide/contract (render source-path [maybe-template-path #f] [maybe-output-path #f])
  ((complete-path?) ((or/c #f complete-path?) (or/c #f complete-path?)) . ->* . (or/c string? bytes?))
  (define output-path (or maybe-output-path (->output-path source-path)))
  (unless output-path
    (raise-argument-error 'render "valid output path" output-path))
  
  (define tests (list has/is-null-source?
                      has/is-preproc-source?
                      has/is-markup-source?
                      has/is-scribble-source?
                      has/is-markdown-source?))
  (define render-procs (list render-null-source
                             render-preproc-source
                             render-markup-or-markdown-source
                             render-scribble-source
                             render-markup-or-markdown-source))
  (define render-proc (for/first ([test (in-list tests)]
                                  [render-proc (in-list render-procs)]
                                  #:when (test source-path))
                        render-proc))
  (unless render-proc
    (raise-argument-error 'render (format "valid rendering function for ~a" source-path) render-proc))
  
  (define template-path (or maybe-template-path (get-template-for source-path output-path)))
  (message (format "rendering: /~a as /~a"
                   (find-relative-path (current-project-root) source-path)
                   (find-relative-path (current-project-root) output-path)))
  ;; output-path and template-path may not have an extension, so check them in order with fallback
  (define render-result (parameterize ([current-poly-target (->symbol (or (get-ext output-path)
                                                                          (and template-path (get-ext template-path))
                                                                          (current-poly-target)))])
                          (apply render-proc (list source-path template-path output-path))))
  ;; wait till last possible moment to store mod dates, because render-proc may also trigger its own subrenders
  ;; e.g., of a template. 
  (update-mod-date-hash! source-path template-path) 
  render-result)


(define (render-null-source source-path . ignored-paths)
  ;((complete-path?) #:rest any/c . ->* . bytes?)
  ;; All this does is copy the source. Hence, "null".
  ;; todo: add test to avoid copying if unnecessary (good idea in case the file is large)
  (file->bytes source-path))


(define (render-scribble-source source-path . _)
  ;((complete-path?) #:rest any/c . ->* . string?)
  (local-require scribble/core scribble/manual (prefix-in scribble- scribble/render))
  (define source-dir (dirname source-path))
  ;; make fresh namespace for scribble rendering (avoids dep/zo caching)
  (time (parameterize ([current-namespace (make-base-namespace)]
                       [current-directory (->complete-path source-dir)])
          (namespace-attach-module (namespace-anchor->namespace render-module-ns) 'scribble/core)
          (namespace-attach-module (namespace-anchor->namespace render-module-ns) 'scribble/manual)
          
          ;; scribble/lp files have their doc export in a 'doc submodule, so check both locations
          (define doc
            [cond
              [(dynamic-require source-path 'doc (λ () #f))]
              [(dynamic-require `(submod ,source-path doc) 'doc (λ () #f))]
              [else #f]])
          ;; BTW this next action has side effects: scribble will copy in its core files if they don't exist.
          (when doc
            (scribble-render (list doc) (list source-path)))))
  (define result (file->string (->output-path source-path)))
  (delete-file (->output-path source-path)) ; because render promises the data, not the side effect
  result)


(define (render-preproc-source source-path . _)
  (time (parameterize ([current-directory (->complete-path (dirname source-path))])
          (render-through-eval (syntax->datum
                                (with-syntax ([SOURCE-PATH source-path])
                                  #'(begin (require pollen/cache)
                                           (cached-doc SOURCE-PATH))))))))


(define (render-markup-or-markdown-source source-path [maybe-template-path #f] [maybe-output-path #f])
  (define output-path (or maybe-output-path (->output-path source-path)))
  (unless output-path
    (raise-argument-error 'render-markup-or-markdown-source "valid output path" output-path))
  (define template-path (or maybe-template-path (get-template-for source-path output-path)))
  (unless template-path
    (raise-argument-error 'render-markup-or-markdown-source "valid template path" template-path))
  (render-from-source-or-output-path template-path) ; because template might have its own preprocessor source
  (define expr-to-eval
    (syntax->datum
     (with-syntax ([DIRECTORY-REQUIRE-FILES (require-directory-require-files source-path)]
                   [DOC-ID (setup:main-export source-path)]
                   [META-ID (setup:meta-export source-path)]
                   [SOURCE-PATH-STRING (path->string source-path)]
                   [CPR (current-project-root)]
                   [HERE-PATH-KEY (setup:here-path-key source-path)]
                   [COMMAND-CHAR (setup:command-char source-path)]
                   [TEMPLATE-PATH (->string template-path)])
       #'(begin 
           (require (for-syntax racket/base)
                    pollen/private/include-template
                    pollen/cache
                    pollen/private/debug
                    pollen/pagetree
                    pollen/core)
           DIRECTORY-REQUIRE-FILES
           (parameterize ([current-pagetree (make-project-pagetree CPR)]
                          [current-metas (cached-metas SOURCE-PATH-STRING)])
             (local-require pollen/template pollen/top)
             (define DOC-ID (cached-doc SOURCE-PATH-STRING))
             (define META-ID (current-metas))
             (define here (path->pagenode (or (select-from-metas 'HERE-PATH-KEY META-ID) 'unknown)))
             (if (bytes? DOC-ID) ; if main export is binary, just pass it through
                 DOC-ID
                 (include-template #:command-char COMMAND-CHAR (file TEMPLATE-PATH))))))))
  ;; set current-directory because include-template wants to work relative to source location
  (time (parameterize ([current-directory (->complete-path (dirname source-path))]) 
          (render-through-eval expr-to-eval))))


(define (templated-source? path)
  (or (markup-source? path) (markdown-source? path)))


(define+provide/contract (get-template-for source-path [maybe-output-path #f])
  ((complete-path?)((or/c #f complete-path?)) . ->* . (or/c #f complete-path?))
  
  (define (file-exists-or-has-source? p) ; p could be #f
    (and p (for/first ([proc (in-list (list values ->preproc-source-path ->null-source-path))]
                       #:when (file-exists? (proc p)))
             p)))
  
  (define (get-template)
    (define output-path (or maybe-output-path (->output-path source-path)))
    (define output-path-ext (or (get-ext output-path) (current-poly-target))) ; output-path may not have an extension
    
    (define (get-template-from-metas)
      (with-handlers ([exn:fail:contract? (λ (e) #f)]) ; in case source-path doesn't work with cached-require
        (parameterize ([current-directory (current-project-root)])
          (define source-metas (cached-metas source-path))
          (define template-name-or-names ; #f or atom or list
            (select-from-metas (setup:template-meta-key source-path) source-metas)) 
          (define template-name (if (list? template-name-or-names)
                                    (findf (λ (tn) (eq? (get-ext tn) output-path-ext)) template-name-or-names)
                                    template-name-or-names))
          (and template-name (build-path (dirname source-path) template-name)))))
    
    (define (get-default-template)
      (and output-path-ext
           (let ([default-template-filename (add-ext (setup:template-prefix source-path) output-path-ext)])
             (find-upward-from source-path default-template-filename file-exists-or-has-source?))))

    (define (get-fallback-template)
      (and output-path-ext
           (build-path (current-server-extras-path)
                       (add-ext (setup:fallback-template-prefix source-path) output-path-ext))))
    
    (or (file-exists-or-has-source? (get-template-from-metas))
        (file-exists-or-has-source? (get-default-template))
        (file-exists-or-has-source? (get-fallback-template))))   
  
  (and (templated-source? source-path) (get-template)))


(module-test-external
 (require pollen/setup sugar/file sugar/coerce)
 (define fallback.html (build-path (current-server-extras-path)
                                   (add-ext (setup:fallback-template-prefix) 'html)))
 (check-equal? (get-template-for (->complete-path "foo.poly.pm")) fallback.html)
 (check-equal? (get-template-for (->complete-path "foo.html.pm")) fallback.html)
 
 (define fallback.svg (build-path (current-server-extras-path)
                                  (add-ext (setup:fallback-template-prefix) 'svg)))
 (parameterize ([current-poly-target 'svg])
   (check-equal? (get-template-for (->complete-path "foo.poly.pm")) fallback.svg)
   (check-equal? (get-template-for (->complete-path "foo.html.pm")) fallback.html))
 
 (define fallback.missing (build-path (current-server-extras-path)
                                      (add-ext (setup:fallback-template-prefix) 'missing)))
 (parameterize ([current-poly-target 'missing])
   (check-false (get-template-for (->complete-path "foo.poly.pm")))
   (check-equal? (get-template-for (->complete-path "foo.html.pm")) fallback.html)))


(define-namespace-anchor render-module-ns)
(define (render-through-eval expr-to-eval)
  (parameterize ([current-namespace (make-base-namespace)]
                 [current-output-port (current-error-port)])
    (namespace-attach-module (namespace-anchor->namespace render-module-ns) 'pollen/setup) ; brings in params
    (eval expr-to-eval)))
