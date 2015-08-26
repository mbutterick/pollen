#lang racket/base
(require racket/path racket/file compiler/cm file/cache sugar/coerce sugar/list "project.rkt" "world.rkt" "rerequire.rkt" "cache-ns.rkt" "debug.rkt" "file.rkt" racket/place)

;; The cache is a hash with paths as keys.
;; The cache values are also hashes, with key/value pairs for that path.

(provide reset-cache preheat-cache cached-require paths->key)


(define (reset-cache [starting-dir (world:current-project-root)])
  (when (or (not (path-string? starting-dir)) (not (directory-exists? starting-dir)))
    (error 'reset-cache (format "~a is not a directory" starting-dir)))
  (for ([path (in-directory starting-dir)]
        #:when (and (directory-exists? path)
                    (equal? (path->string (car (reverse (explode-path path)))) (world:current-cache-dir-name))))
       (message (format "removing cache directory: ~a" path))
       (delete-directory/files path)))


(define (preheat-cache [starting-dir (world:current-project-root)])
  (when (or (not (path-string? starting-dir)) (not (directory-exists? starting-dir)))
    (error 'preheat-cache (format "~a is not a directory" starting-dir)))
  
  (define max-places 8) ; number of parallel processes to spawn at a time
  
  (define paths-that-should-be-cached (for/list ([path (in-directory starting-dir)]
                                                 #:when (or (preproc-source? path)
                                                            (markup-source? path)
                                                            (markdown-source? path)
                                                            (pagetree-source? path)))
                                                path))
  
  ;; if a file is already in the cache, no need to hit it again.
  ;; this allows partially completed preheat jobs to resume.
  (define uncached-paths (filter
                          (λ(path)
                            ;; #t = not cached; #f = already cached
                            ;; seems like it would be slow to load cache.rktd but it's not.
                            (define-values (_ private-cache-dir) (make-cache-dirs path))
                            (define cache-db-file (build-path private-cache-dir "cache.rktd"))
                            (cond
                              [(not (file-exists? cache-db-file)) #t]
                              [else (define cache-db (file->value cache-db-file))
                                    (not (hash-has-key? cache-db (paths->key path)))])) paths-that-should-be-cached))
  
  ;; compile a path inside a place (= parallel processing)
  (define (path-into-place path)
    (message (format "caching: ~a" (find-relative-path starting-dir path)))
    (define p (place ch
                     (define path (place-channel-get ch))
                     (define-values (path-dir path-name _) (split-path path))
                     (message (format "compiling: ~a" path-name))
                     ;; use #f to signal compile error. Otherwise allow errors to pass.
                     (define result (with-handlers ([exn:fail? (λ _ (message "~a failed" path-name) #f)])
                                      (path->hash path)))
                     (place-channel-put ch result)))
    (place-channel-put p path)
    p)
  
  ;; compile the paths in groups, so they can be incrementally saved.
  ;; that way, if there's a failure, the progress is preserved.
  ;; but the slowest file in a group will prevent further progress.
  (for ([path-group (in-list (slice-at uncached-paths max-places))])
       (define path-places (map path-into-place path-group))
       (for ([path (in-list path-group)]
             [ppl (in-list path-places)])
            (define result (place-channel-get ppl))
            (when result ; #f is used to signal compile error
              (cache-ref! (paths->key path) (λ _ result))))))


(define (paths->key source-path [template-path #f])
  ;; key is list of file + mod-time pairs, use #f for missing
  (define path-strings (append (list source-path)
                               (append (list template-path) ; is either path or #f
                                       (->list (get-directory-require-files source-path))))) ; is either list of files or (list #f)
  ;; can't use relative paths for cache keys because source files include `here-path` which is absolute.
  ;; problem is that cache could appear valid on another filesystem (based on relative pathnames & mod dates)
  ;; but would actually be invalid (because the `here-path` names are wrong).
  (define path+mod-time-pairs
    (map (λ(ps) (and ps (let ([cp (->complete-path ps)])
                          (cons (path->string cp) (file-or-directory-modify-seconds cp))))) path-strings))
  path+mod-time-pairs)


(define (key->source-path key)
  (car (car key)))


(define (path->hash path)
  ;; new namespace forces dynamic-require to re-instantiate 'path'
  ;; otherwise it gets cached in current namespace.
  (define drfs (get-directory-require-files path))
  (for-each managed-compile-zo (or drfs null))
  (define-values (path-dir path-name _) (split-path path))
  (apply hash
         (let ([doc-key (world:current-main-export)]
               [meta-key (world:current-meta-export)])
           (parameterize ([current-namespace (make-base-namespace)]
                          [current-directory path-dir])
             ;; I monkeyed around with using the metas submodule to pull out the metas (for speed)
             ;; but in practice most files get their doc requested too.
             ;; so it's just simpler to get both at once and be done with it.
             ;; the savings of avoiding two cache fetches at the outset outweighs
             ;; the benefit of not reloading doc when you just need metas.
             (list doc-key (dynamic-require path doc-key) meta-key (dynamic-require path meta-key))))))


(define (my-make-directory* dir)
  (let-values ([(base name dir?) (split-path dir)])
    (when (and (path? base) (not (directory-exists? base)))
      (my-make-directory* base))
    (unless (directory-exists? dir)
      (with-handlers ([exn:fail:filesystem:exists? void])
        (make-directory dir)))))

(define (make-cache-dirs path)
  (define-values (path-dir path-filename _) (split-path path))
  (define cache-dir (build-path path-dir (world:current-cache-dir-name)))
  (define private-cache-dir (build-path cache-dir "private"))
  (my-make-directory* private-cache-dir) ; will also make cache-dir, if needed
  (values cache-dir private-cache-dir))

(define ram-cache (make-hash))


(define (cache-ref! key path-hash-thunk)
  (define path (key->source-path key))
  (define-values (cache-dir private-cache-dir) (make-cache-dirs path))
  (define-values (path-dir path-filename _) (split-path path))
  (define dest-file (build-path cache-dir (format "~a.rktd" path-filename)))
  (cache-file dest-file
              #:exists-ok? #t
              key
              private-cache-dir
              (λ _
                (write-to-file (path-hash-thunk) dest-file #:exists 'replace))
              #:max-cache-size (world:current-compile-cache-max-size))
  (file->value dest-file))

(define (cached-require path-string subkey)  
  (define path (with-handlers ([exn:fail? (λ _ (error 'cached-require (format "~a is not a valid path" path-string)))])
                 (->complete-path path-string)))
  
  (when (not (file-exists? path))
    (error (format "cached-require: ~a does not exist" path)))
  
  (cond
    [(world:current-compile-cache-active)
     (define key (paths->key path))
     (hash-ref (hash-ref! ram-cache key (λ _
                                          (cache-ref! key (λ _ (path->hash path))))) subkey)]
    [else (parameterize ([current-namespace (make-base-namespace)])
            (dynamic-require path subkey))]))