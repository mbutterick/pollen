#lang racket/base
(require racket/file
         racket/path
         racket/match
         racket/place
         racket/list
         racket/dict
         sugar/test
         sugar/define
         sugar/file
         sugar/coerce
         sugar/list
         version/utils
         "private/file-utils.rkt"
         "cache.rkt"
         "private/log.rkt"
         "private/project.rkt"
         "private/cache-utils.rkt"
         "pagetree.rkt"
         "core.rkt"
         "setup.rkt")

;; used to track renders according to modification dates of component files
(define mod-date-hash #false)

;; when you want to generate everything fresh. 
;; render functions will always go when no mod-date is found.
(define (reset-mod-date-hash!) (set! mod-date-hash (make-hash)))
(reset-mod-date-hash!)

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
  (hash-set! mod-date-hash (paths->key source-path template-path) #true))

(define (mod-date-missing-or-changed? source-path template-path)
  (not (hash-has-key? mod-date-hash (paths->key source-path template-path))))

(define (parallel-render source-paths-in job-count-arg)
  ;; if jobs are already in the cache, pull them out before assigning workers
  ;; using worker to fetch from cache is slower
  (define-values (uncached-source-paths previously-cached-jobs)
    (for/fold ([usps null]
               [pcjs null])
              ([path (in-list source-paths-in)])
      (match (with-handlers ([(λ (x) (eq? x 'cache-miss)) values])
               (render-to-file-if-needed path #f #f (λ () (raise 'cache-miss))))
        ['cache-miss (values (cons path usps) pcjs)]
        [_ (values usps (cons (cons path #true) pcjs))])))
  
  (define job-count
    (min
     (length uncached-source-paths)
     (match job-count-arg
       [#true (processor-count)]
       [(? exact-positive-integer? count) count]
       [_ (raise-user-error 'render-batch "~a is not an exact positive integer or #true" job-count-arg)])))
  
  ;; initialize the workers
  (define worker-evts
    (for/list ([wpidx (in-range job-count)])
              (define wp (place ch
                                (let loop ()
                                  (match-define (cons path poly-target)
                                    (place-channel-put/get ch (list 'wants-job)))
                                  (parameterize ([current-poly-target poly-target])
                                    (place-channel-put/get ch (list 'wants-lock (->output-path path)))
                                    ;; trap any exceptions and pass them back as crashed jobs.
                                    ;; otherwise, a crashed rendering place can't recover, and the parallel job will be stuck.
                                    (with-handlers ([exn:fail? (λ (e) (place-channel-put ch (list 'crashed-job path #f)))])
                                      (match-define-values (_ _ ms _)
                                        ;; we don't use `render-to-file-if-needed` because we've already checked the render cache
                                        ;; if we reached this point, we know we need a render
                                        (time-apply render-to-file (list path)))
                                      (place-channel-put ch (list 'finished-job path ms))))
                                  (loop))))
              (handle-evt wp (λ (val) (list* wpidx wp val)))))
     
  (define poly-target (current-poly-target))

  ;; `locks` and `blocks` are (listof (cons/c evt? path?))
  (let loop ([source-paths (reverse uncached-source-paths)]
             [locks-in null]
             [blocks-in null]
             ;; `completed-jobs` is (listof (cons/c path? boolean?))
             [completed-jobs previously-cached-jobs]
             [completed-job-count (length previously-cached-jobs)])
    ;; try to unblock blocked workers
    (define-values (locks blocks)
      (for/fold ([locks locks-in]
                 [blocks null])
                ([block (in-list blocks-in)])
        (match-define (cons wp path) block)
        (cond
          [(member path (dict-values locks))
           (values locks (cons block blocks))]
          [else
           (place-channel-put wp 'lock-approved)
           (values (cons block locks) blocks)])))
    (cond
      [(eq? completed-job-count (length source-paths-in))
       ;; second bite at the apple for crashed jobs.
       ;; 1) many crashes that arise in parallel rendering are
       ;; a result of concurrency issues (e.g. shared files not being readable at the right moment).
       ;; That is, they do not appear under serial rendering.
       ;; 2) even if a crash is legit (that is, there is a real flaw in the source)
       ;; and should be raised, we don't want to do it inside a parallel-rendering `place`
       ;; because then the place will never return, and the whole parallel job will never finish.
       ;; so we take the list of crashed jobs and try rendering them again serially.
       ;; if it was a concurrency-related error, it will disappear.
       ;; if it was a legit error, the render will stop and print a trace.
       ;; crashed jobs are completed jobs that weren't finished
       (for/list ([(path finished?) (in-dict completed-jobs)]
                  #:unless finished?)
                 path)]
      [else
       (match (apply sync worker-evts)
         [(list wpidx wp 'wants-job)
          (match source-paths
            [(? null?) (loop null locks blocks completed-jobs completed-job-count)]
            [(cons path rest)
             (place-channel-put wp (cons path poly-target))
             (loop rest locks blocks completed-jobs completed-job-count)])]
         [(list wpidx wp (and (or 'finished-job 'crashed-job) tag) path ms)
          (match tag
            ['finished-job
             (message
              (format "rendered parallel @ job ~a /~a ~a"
                      (add1 wpidx)
                      (find-relative-path (current-project-root) (->output-path path))
                      (if (< ms 1000) (format "(~a ms)" ms) (format "(~a s)" (/ ms 1000.0)))))]
            [_
             (message
              (format "render crash @ job ~a /~a (retry pending)"
                      (add1 wpidx)
                      (find-relative-path (current-project-root) (->output-path path))))])
          (loop source-paths
                (match (assoc wp locks)
                  [#false locks]
                  [lock (remove lock locks)])
                blocks
                (cons (cons path (eq? tag 'finished-job)) completed-jobs)
                (add1 completed-job-count))]
         [(list wpidx wp 'wants-lock path)
          (loop source-paths locks (append blocks (list (cons wp path))) completed-jobs completed-job-count)])])))

(define+provide/contract (render-batch #:parallel [wants-parallel-render? #false]
                                       #:dry-run [wants-dry-run? #false] . paths-in)
  ((#:parallel any/c) (#:dry-run boolean?) #:rest (listof pathish?) . ->* . void?)
  ;; Why not just (for-each render ...)?
  ;; Because certain files will pass through multiple times (e.g., templates)
  ;; And with render, they would be rendered repeatedly.
  ;; Using reset-modification-dates is sort of like session control.
  (reset-mod-date-hash!)
  (define expanded-source-paths
    (let loop ([paths paths-in] [acc null])
      (match paths
        [(? null?) (sort (remove-duplicates acc) string<? #:key path->string)]
        [(cons path rest)
         (match (->complete-path path)
           [(? pagetree-source? pt)
            (loop (append (pagetree->paths pt) rest) acc)]
           [(app ->source-path (and (not #false) (? file-exists?) sp))
            (loop rest (cons sp acc))]
           [_ (loop rest acc)])])))
  (cond
    [(null? expanded-source-paths) (message "[no paths to render]")]
    [wants-dry-run? (for-each message expanded-source-paths)]
    [else (for-each render-to-file-if-needed
                    (match wants-parallel-render?
                      ;; returns crashed jobs for serial rendering
                      [#false expanded-source-paths]
                      [jobs-arg (parallel-render expanded-source-paths jobs-arg)]))]))

(define (pagetree->paths pagetree-or-path)    
  (parameterize ([current-directory (current-project-root)])
    (map ->complete-path (pagetree->list (match pagetree-or-path
                                           [(? pagetree? pt) pt]
                                           [_ (cached-doc pagetree-or-path)])))))

(define+provide/contract (render-pagenodes pagetree-or-path)
  ((or/c pagetree? pathish?) . -> . void?)
  (apply render-batch (pagetree->paths pagetree-or-path)))

(define+provide/contract (render-from-source-or-output-path so-pathish)
  (pathish? . -> . void?)
  (define so-path (->complete-path so-pathish))
  (define-values (sp op) (->source+output-paths so-path))
  (cond
    [(and sp op) (render-to-file-if-needed sp #false op)]
    [(pagetree-source? so-path) (render-pagenodes so-path)]))

(define render-ram-cache (make-hash))

;; note that output and template order is reversed from typical
(define (render-to-file-base caller
                             force?
                             source-path
                             maybe-output-path
                             maybe-template-path
                             maybe-render-thunk)
  (unless (file-exists? source-path)
    (raise-user-error caller "~a is not an existing source path" source-path))
  (define output-path (cond
                        [maybe-output-path]
                        [(->output-path source-path)]
                        [else (raise-user-error caller "~a is not a valid output path" maybe-output-path)]))
  (define template-path (cond
                          [maybe-template-path]
                          [(get-template-for source-path output-path)]
                          [else #false]))
  (define render-cache-activated? (setup:render-cache-active source-path))
  (define render-needed?
    (cond
      [force?]
      [(not (file-exists? output-path)) 'file-missing]
      [(mod-date-missing-or-changed? source-path template-path) 'mod-key-missing-or-changed]
      [(not render-cache-activated?) 'render-cache-deactivated]
      [else #false]))
  (when render-needed?
    (define render-thunk (or maybe-render-thunk (λ () (render source-path template-path output-path)))) ; returns either string or bytes
    (define render-result
      (cond
        [render-cache-activated?
         (define key (paths->key source-path template-path output-path))
         (hash-ref! render-ram-cache
                    ;; within a session, this will prevent repeat players like "template.html.p"
                    ;; from hitting the file cache repeatedly
                    key
                    (cache-ref! key
                                render-thunk
                                #:dest-path 'output
                                #:notify-cache-use
                                (λ (str)
                                  (message (format "from cache /~a"
                                                   (find-relative-path (current-project-root) output-path))))))]
        [else (render-thunk)]))
    (display-to-file render-result
                     output-path
                     #:exists 'replace
                     #:mode (if (string? render-result) 'text 'binary))))

(define+provide/contract (render-to-file-if-needed source-path [maybe-template-path #f] [maybe-output-path #f] [maybe-render-thunk #f])
  ((complete-path?) ((or/c #f complete-path?) (or/c #f complete-path?) (or/c #f procedure?)) . ->* . void?)
  (render-to-file-base 'render-to-file-if-needed #f source-path maybe-output-path maybe-template-path maybe-render-thunk))

(define+provide/contract (render-to-file source-path [maybe-template-path #f] [maybe-output-path #f] [maybe-render-thunk #f])
  ((complete-path?) ((or/c #f complete-path?) (or/c #f complete-path?) (or/c #f procedure?)) . ->* . void?)
  (render-to-file-base 'render-to-file #t source-path maybe-output-path maybe-template-path maybe-render-thunk))

(define+provide/contract (render source-path [maybe-template-path #f] [maybe-output-path #f])
  ((complete-path?) ((or/c #f complete-path?) (or/c #f complete-path?)) . ->* . (or/c string? bytes?))
  (unless (file-exists? source-path)
    (raise-user-error 'render "~a is not an existing source path" source-path))
  (define output-path (cond
                        [maybe-output-path]
                        [(->output-path source-path)]
                        [else (raise-user-error 'render "~a is not a valid output path" maybe-output-path)]))
  (define render-proc
    (match source-path
      [(? has/is-null-source?) render-null-source]
      [(? has/is-preproc-source?) render-preproc-source]
      [(? has/is-markup-source?) render-markup-or-markdown-source]
      [(? has/is-scribble-source?) render-scribble-source]
      [(? has/is-markdown-source?) render-markup-or-markdown-source]
      [_ (raise-user-error 'render "valid rendering function for ~a" source-path)]))
  
  (define template-path (cond
                          [maybe-template-path]
                          [(get-template-for source-path output-path)]
                          [else #false]))
  
  ;; output-path and template-path may not have an extension, so check them in order with fallback

  (match-define-values ((cons render-result _) _ ms _)
    (parameterize ([current-directory (->complete-path (dirname source-path))]
                   [current-poly-target (->symbol (or (get-ext output-path)
                                                      (and template-path (get-ext template-path))
                                                      (current-poly-target)))]
                   [current-render-source source-path])
      (message (format "rendering /~a~a"
                       (find-relative-path (current-project-root) source-path)
                       (if (has-inner-poly-ext? source-path) (format " as ~a" (current-poly-target)) "")))
      (time-apply render-proc (list source-path template-path output-path))))
  ;; wait till last possible moment to store mod dates, because render-proc may also trigger its own subrenders
  ;; e.g., of a template.
  (message (apply format "rendered /~a (~a ~a)"
                  (find-relative-path (current-project-root) output-path)
                  (if (< ms 1000) (list ms "ms") (list (/ ms 1000.0) "s"))))
  (update-mod-date-hash! source-path template-path) 
  render-result)

(define (render-null-source source-path . ignored-paths)
  ;((complete-path?) #:rest any/c . ->* . bytes?)
  ;; All this does is copy the source. Hence, "null".
  ;; todo: add test to avoid copying if unnecessary (good idea in case the file is large)
  (file->bytes source-path))

(define-namespace-anchor render-module-ns)

(define (render-scribble-source source-path . _)
  ;((complete-path?) #:rest any/c . ->* . string?)
  (local-require scribble/core scribble/manual (prefix-in scribble- scribble/render))
  (define source-dir (dirname source-path))
  ;; make fresh namespace for scribble rendering (avoids dep/zo caching)
  (parameterize ([current-namespace (make-base-namespace)])
    (define outer-ns (namespace-anchor->namespace render-module-ns))
    (namespace-attach-module outer-ns 'scribble/core)
    (namespace-attach-module outer-ns 'scribble/manual)
    ;; scribble/lp files have their doc export in a 'doc submodule, so check both locations
    (match (cond
             [(dynamic-require source-path 'doc (λ () #false))]
             [(dynamic-require `(submod ,source-path doc) 'doc (λ () #false))]
             [else #false])
      ;; BTW this next action has side effects: scribble will copy in its core files if they don't exist.
      [(? part? doc) (scribble-render (list doc) (list source-path))]
      [_ (void)]))
  (define op (->output-path source-path))
  (begin0 ; because render promises the data, not the side effect
    (file->string op)
    (delete-file op)))

(define (render-preproc-source source-path . _)
  (cached-doc source-path))

(define (render-markup-or-markdown-source source-path [maybe-template-path #f] [maybe-output-path #f])
  (define output-path
    (cond
      [maybe-output-path]
      [(->output-path source-path)]
      [else (raise-user-error 'render-markup-or-markdown-source "~a is not a valid output path" maybe-output-path)]))
  (define template-path
    (cond
      [maybe-template-path]
      [(get-template-for source-path output-path)]
      [else (raise-user-error 'render-markup-or-markdown-source
                              "couldn't find template for target .~a"
                              (current-poly-target))]))

  ;; use a temp file so that multiple (possibly parallel) renders
  ;; do not compete for write access to the same template
  (define temp-template (make-temporary-file "pollentmp~a" (cond
                                                             [(->source-path template-path)]
                                                             [template-path]
                                                             [else #false])))
  (render-from-source-or-output-path temp-template) ; because template might have its own preprocessor source
  (parameterize ([current-output-port (current-error-port)]
                 [current-namespace (make-base-namespace)])
    (define outer-ns (namespace-anchor->namespace render-module-ns))
    (namespace-attach-module outer-ns 'pollen/setup)
    (begin0
      (eval (with-syntax ([MODNAME (gensym)]
                          [SOURCE-PATH-STRING (->string source-path)]
                          [TEMPLATE-PATH-STRING (->string temp-template)])
              #'(begin
                  (module MODNAME pollen/private/render-helper
                    #:source SOURCE-PATH-STRING
                    #:template TEMPLATE-PATH-STRING
                    #:result-id result)
                  (require 'MODNAME)
                  result)))
      (delete-file temp-template))))

(define (file-exists-or-has-source? path) ; path could be #f
  (and path (for/first ([proc (in-list (list values ->preproc-source-path ->null-source-path))]
                        #:when (file-exists? (proc path)))
                       path)))

(define (get-template-from-metas source-path output-path-ext)
  (with-handlers ([exn:fail:contract? (λ (e) #f)]) ; in case source-path doesn't work with cached-require
    (parameterize ([current-directory (current-project-root)])
      (define source-metas (cached-metas source-path))
      (define template-name-or-names ; #f or atom or list
        (select-from-metas (setup:template-meta-key source-path) source-metas)) 
      (define template-name (if (list? template-name-or-names)
                                (findf (λ (tn) (eq? (get-ext tn) output-path-ext)) template-name-or-names)
                                template-name-or-names))
      (and template-name (simplify-path (cleanse-path (build-path (dirname source-path) template-name)))))))

(define (get-default-template source-path output-path-ext)
  (and output-path-ext
       (let ([default-template-filename (add-ext (setup:template-prefix source-path) output-path-ext)])
         (find-upward-from source-path default-template-filename file-exists-or-has-source?))))

(define (get-fallback-template source-path output-path-ext)
  (and output-path-ext
       (build-path (current-server-extras-path)
                   (add-ext (setup:fallback-template-prefix source-path) output-path-ext))))

(define+provide/contract (get-template-for source-path [maybe-output-path #f])
  ((complete-path?)((or/c #f complete-path?)) . ->* . (or/c #f complete-path?))
  (match source-path
    [(or (? markup-source?) (? markdown-source?))
     (define output-path (cond
                           [maybe-output-path]
                           [(->output-path source-path)]
                           [else #false]))
     ;; output-path may not have an extension
     (define output-path-ext (cond
                               [(get-ext output-path)]
                               [(current-poly-target)]
                               [else #false]))
     (for/or ([proc (list get-template-from-metas get-default-template get-fallback-template)])
             (file-exists-or-has-source? (proc source-path output-path-ext)))]
    [_ #false]))

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

