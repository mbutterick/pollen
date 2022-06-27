#lang racket/base
(require racket/file
         racket/path
         racket/match
         racket/string
         racket/format
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
                   (map simple-form-path (filter (λ (name) (regexp-match "sample-" name)) (directory-list ".")))))
 (define-values (sample-01 sample-02 sample-03) (apply values samples)))

;; each key for mod-date-hash is a list of file / mod-date pairs (using pollen/cache keymaking function)
;; when a file is rendered, a new key is stored in the hash (with trivial value #t)
;; after that, the hash-key-comparision routine intrinsic to hash lookup
;; can be used to test whether a render is obsolete.
;; create a new key with current files. If the key is in the hash, the render has happened.
;; if not, a new render is needed.
(define (update-mod-date-hash! source-path template-path)
  (hash-set! mod-date-hash (paths->key 'output source-path template-path) #true))

(define (mod-date-missing-or-changed? source-path template-path)
  (not (hash-has-key? mod-date-hash (paths->key 'output source-path template-path))))

(struct $job (source output) #:transparent)
(struct $jobresult (job finished-successfully) #:transparent)
(define (parallel-render jobs-in worker-count-arg)
  ;; if jobs are already in the cache, pull them out before assigning workers
  ;; using worker to fetch from cache is slower
  (define-values (uncached-jobs previously-cached-jobs)
    (for/fold ([ujobs null]
               [pcjobs null])
              ([job (in-list jobs-in)])
      (match (let/ec exit
               (define template-path
                 (cache-ref! (template-cache-key ($job-source job) ($job-output job)) (λ () (exit 'template-miss))))
               (render-to-file-if-needed ($job-source job) template-path ($job-output job) (λ () (exit 'render-miss))))
        [(? symbol? sym) (values (cons job ujobs) pcjobs)]
        [_ (values ujobs (cons ($jobresult job #true) pcjobs))])))

  (define worker-count
    (min
     (length uncached-jobs)
     (match worker-count-arg
       [#true (processor-count)]
       [(? exact-positive-integer? count) count]
       [_ (raise-user-error 'render-batch "~a is not an exact positive integer or #true" worker-count-arg)])))
  
  ;; initialize the workers
  (define worker-evts
    (for/list ([wpidx (in-range worker-count)])
              (define wp
                (place ch
                       (let loop ()
                         (match-define (list project-root source-path output-path poly-target)
                           (place-channel-put/get ch (list 'wants-job)))
                         ;; we manually propagate our parameter values for
                         ;; current-project-root and current-poly-target
                         ;; because parameter values are not automatically shared
                         ;; between parallel threads.
                         (parameterize ([current-project-root project-root]
                                        [current-poly-target poly-target])
                           (place-channel-put/get ch (list 'wants-lock output-path))
                           ;; trap any exceptions and pass them back as crashed jobs.
                           ;; otherwise, a crashed rendering place can't recover, and the parallel job will be stuck.
                           (place-channel-put ch
                                              (cons
                                               ;; when rendering fails, first argument is the exception message
                                               (with-handlers ([exn:fail? (λ (e) (exn-message e))])
                                                 (match-define-values (_ _ ms _)
                                                   ;; we don't use `render-to-file-if-needed` because we've already checked the render cache
                                                   ;; if we reached this point, we know we need a render
                                                   (time-apply render-to-file (list source-path #f output-path)))
                                                 ;; when rendering succeeds, first argument is rendering time in ms
                                                 ms)
                                               (list source-path output-path))))
                         (loop))))
              (handle-evt wp (λ (val) (list* wpidx wp val)))))
     
  (define poly-target (current-poly-target))

  (struct $lock (worker path) #:transparent)
  ;; `locks` and `blocks` are (listof $lock)
  (let loop ([jobs (reverse uncached-jobs)]
             [locks-in null]
             [blocks-in null]
             [completed-job-results previously-cached-jobs] ; (listof jobresult)
             [completed-job-count (length previously-cached-jobs)])
    ;; try to unblock blocked workers
    (define-values (locks blocks)
      (for/fold ([locks locks-in]
                 [blocks null])
                ([block (in-list blocks-in)])
        (match-define ($lock wp path) block)
        (cond
          [(member path (map $lock-path locks))
           (values locks (cons block blocks))]
          [else
           (place-channel-put wp 'lock-approved)
           (values (cons block locks) blocks)])))
    (cond
      [(eq? completed-job-count (length jobs-in))
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
       (for/list ([jr (in-list completed-job-results)]
                  #:unless ($jobresult-finished-successfully jr))
                 ($jobresult-job jr))]
      [else
       (match (apply sync worker-evts)
         [(list wpidx wp 'wants-job)
          (match jobs
            [(? null?) (loop null locks blocks completed-job-results completed-job-count)]
            [(cons ($job source-path output-path) rest)
             (place-channel-put wp (list (current-project-root) source-path output-path poly-target))
             (loop rest locks blocks completed-job-results completed-job-count)])]
         [(list wpidx wp status-arg source-path output-path)
          ;; if the render was successful, the status arg is a number representing milliseconds spent rendering.
          ;; if not, the status argument is the exception message.
          (define job-finished? (exact-nonnegative-integer? status-arg))
          (match status-arg
            [ms #:when job-finished?
                (message
                 (format "rendered @ job ~a /~a ~a"
                         (~r (add1 wpidx) #:min-width (string-length (~r worker-count)) #:pad-string " ")
                         (find-relative-path (current-project-root) output-path)
                         (if (< ms 1000) (format "(~a ms)" ms) (format "(~a s)" (/ ms 1000.0)))))]
            [(? string? exn-msg)
             (message
              (format "render crash @ job ~a /~a (retry pending)\n  because ~a"
                      (add1 wpidx)
                      (find-relative-path (current-project-root) output-path)
                      exn-msg))]
            [_ (raise-result-error 'render "exact-nonnegative-integer or string" status-arg)])
          (loop jobs
                (match (findf (λ (lock) (equal? ($lock-worker lock) wp)) locks)
                  [#false locks]
                  [lock (remove lock locks)])
                blocks
                (let ([jr ($jobresult ($job source-path output-path) job-finished?)])
                  (cons jr completed-job-results))
                (add1 completed-job-count))]
         [(list wpidx wp 'wants-lock output-path)
          (loop jobs locks (append blocks (list ($lock wp output-path))) completed-job-results completed-job-count)])])))

(define current-null-output? (make-parameter #f))

(define+provide/contract (render-batch #:parallel [wants-parallel-render? #false]
                                       #:special [special-output #false]
                                       #:output-paths [output-paths-in #false] . paths-in)
  (() (#:parallel any/c
    #:special (or/c boolean? symbol?)
    #:output-paths (or/c #false (listof pathish?)))
   #:rest (listof pathish?) . ->* . void?)
  ;; Why not just (for-each render ...)?
  ;; Because certain files will pass through multiple times (e.g., templates)
  ;; And with render, they would be rendered repeatedly.
  ;; Using reset-modification-dates is sort of like session control.
  (reset-mod-date-hash!)
  ;; we need to handle output-paths in parallel
  ;; because `raco pollen render` can take an output path for poly source.
  ;; meaning, if source is "test.poly.pm" and we get `raco pollen render test.txt`,
  ;; then the output path argument should force .txt rendering, regardless of `current-poly-target` setting
  ;; so the output path may contain information we need that we can't necessarily derive from the source path.

  (define all-jobs
    ;; we generate the output paths in parallel with the source paths
    ;; rather than afterward, because
    ;; for poly files we want to be able to look at
    ;; the original path provided as an argument
    ;; but the path arguments might also include pagetrees,
    ;; which expand to multiple files.
    ;; so this keeps everything correlated correctly.
    (cond
      [(and output-paths-in (= (length paths-in) (length output-paths-in)))
       ;; explicit list of paths: create jobs directly
       (for/list ([path (in-list paths-in)]
                  [output-path (in-list output-paths-in)])
                 ($job path output-path))]
      [else
       (let loop ([paths paths-in] [sps null] [ops null]) 
         (match paths
           [(? null?)
            ;; it's possible that we have multiple output names for one poly file
            ;; so after we expand, we only remove duplicates where both the source and dest in the pair
            ;; are the same
            (let* ([pairs (remove-duplicates (map cons sps ops))]
                   [pairs (sort pairs path<? #:key car)]
                   [pairs (sort pairs path<? #:key cdr)])
              (for/list ([pr (in-list pairs)])
                        ($job (car pr) (cdr pr))))]
           [(cons path rest)
            (match (->complete-path path)
              [(? pagetree-source? pt)
               (loop (append (pagetree->paths pt) rest) sps ops)]
              [(app ->source-path sp) #:when (and sp (file-exists? sp))
                                      (define op (match path
                                                   [(== (->output-path path)) path]
                                                   [_ (->output-path sp)]))
                                      (loop rest (cons sp sps) (cons op ops))]
              [_ (loop rest sps ops)])]))]))
  (cond
    [(null? all-jobs) (message "[no paths to render]")]
    [(eq? special-output 'dry-run) (for-each message (map $job-source all-jobs))]
    [else
     (parameterize ([current-null-output? (eq? special-output 'null)])
       (for-each (λ (job) (render-to-file-if-needed ($job-source job) #f ($job-output job)))
                 (match wants-parallel-render?
                   ;; returns crashed jobs for serial rendering
                   [#false all-jobs]
                   [worker-count-arg (parallel-render all-jobs worker-count-arg)])))]))

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

(define ram-cache (make-hash))

(define (get-external-render-proc v)
  (match v
    [(list (? module-path? mod) (? symbol? render-proc-id))
     (with-handlers ([exn:fail:filesystem:missing-module?
                      (λ (e) (raise
                              (exn:fail:contract (string-replace (exn-message e) "standard-module-name-resolver" "external-renderer")
                                                 (exn-continuation-marks e))))]
                     [exn:fail:contract? ;; raised if dynamic-require can't find render-proc-id
                      (λ (e) (raise
                              (exn:fail:contract (string-replace (exn-message e) "dynamic-require" "external-renderer")
                                                 (exn-continuation-marks e))))])
       (dynamic-require mod render-proc-id))]
    [_ (raise-argument-error 'external-renderer "value in the form '(module-path proc-id)" v)]))

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
    (define render-thunk (or maybe-render-thunk
                             (λ () ((or (let ([val (setup:external-renderer)])
                                          (and val (get-external-render-proc val)))
                                        render)
                                    source-path template-path output-path)))) ; returns either string or bytes
    (define render-result
      (cond
        [render-cache-activated?
         (define key (paths->key 'output source-path template-path output-path))
         (hash-ref! ram-cache
                    ;; within a session, this will prevent repeat players like "template.html.p"
                    ;; from hitting the file cache repeatedly
                    key
                    (λ ()
                      (cache-ref! key
                                  render-thunk
                                  #:notify-cache-use
                                  (λ (str)
                                    (message (format "from cache /~a"
                                                     (find-relative-path (current-project-root) output-path)))))))]
        [else (render-thunk)]))
    (unless (current-null-output?)
      (display-to-file render-result
                       output-path
                       #:exists 'replace
                       #:mode (if (string? render-result) 'text 'binary)))))

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
        (select-from-metas pollen-template-meta-key source-metas)) 
      (define template-name (if (list? template-name-or-names)
                                (findf (λ (tn) (eq? (get-ext tn) output-path-ext)) template-name-or-names)
                                template-name-or-names))
      (and template-name (simplify-path (cleanse-path (build-path (dirname source-path) template-name)))))))

(define (get-default-template source-path output-path-ext)
  (and output-path-ext
       (let ([default-template-filename (add-ext pollen-template-prefix output-path-ext)])
         (find-upward-from source-path default-template-filename file-exists-or-has-source?))))

(define (get-fallback-template source-path output-path-ext)
  (and output-path-ext
       (build-path (current-server-extras-path)
                   (add-ext pollen-fallback-template-prefix output-path-ext))))

(define (template-cache-key source-path output-path)
  (paths->key 'template source-path (current-poly-target) output-path))
  
(define+provide/contract (get-template-for source-path [maybe-output-path #f])
  ((complete-path?)((or/c #f complete-path?)) . ->* . (or/c #f complete-path?))
  (define output-path (cond
                        [maybe-output-path]
                        [(->output-path source-path)]
                        [else #false]))
  (define (cache-thunk)
    (match source-path
      [(or (? markup-source?) (? markdown-source?))
       ;; output-path may not have an extension
       (define output-path-ext (cond
                                 [(get-ext output-path)]
                                 [(current-poly-target)]
                                 [else #false]))
       (for/or ([proc (list get-template-from-metas
                            get-default-template
                            get-fallback-template)])
               (file-exists-or-has-source? (proc source-path output-path-ext)))]
      [_ #false]))
  (cond
    [(or (current-session-interactive?) (not (setup:render-cache-active source-path)))
     ;; don't cache templates in interactive session, for fresher reloads
     ;; this makes it possible to add template and have it show up in next render
     (cache-thunk)]
     ;; otherwise, within a rendering session, this will prevent repeat players like "template.html.p"
     ;; from hitting the file cache repeatedly
     [else 
      (define key (template-cache-key source-path output-path))
      (hash-ref! ram-cache key (λ () (cache-ref! key cache-thunk)))]))

#;(module-test-external
 (require pollen/setup sugar/file sugar/coerce)
 (define fallback.html (build-path (current-server-extras-path)
                                   (add-ext pollen-fallback-template-prefix 'html)))
 (check-equal? (get-template-for (->complete-path "foo.poly.pm")) fallback.html)
 (check-equal? (get-template-for (->complete-path "foo.html.pm")) fallback.html)
 
 (define fallback.svg (build-path (current-server-extras-path)
                                  (add-ext pollen-fallback-template-prefix 'svg)))
 (parameterize ([current-poly-target 'svg])
   (check-equal? (get-template-for (->complete-path "foo.poly.pm")) fallback.svg)
   (check-equal? (get-template-for (->complete-path "foo.html.pm")) fallback.html))
 
 (define fallback.missing (build-path (current-server-extras-path)
                                      (add-ext pollen-fallback-template-prefix 'missing)))
 (parameterize ([current-poly-target 'missing])
   (check-false (get-template-for (->complete-path "foo.poly.pm")))
   (check-equal? (get-template-for (->complete-path "foo.html.pm")) fallback.html)))

