#lang racket/base
(require "file-utils.rkt" racket/file "cache-utils.rkt" "debug.rkt" racket/path racket/place sugar/list)
(provide preheat-cache)

(define (preheat-cache starting-dir)
  (unless (and (path-string? starting-dir) (directory-exists? starting-dir))
    (error 'preheat-cache (format "~a is not a directory" starting-dir)))
  
  (define max-places (processor-count)) ; number of parallel processes to spawn at a time
  
  (define paths-that-should-be-cached (for/list ([path (in-directory starting-dir)]
                                                 #:when (for/or ([proc (in-list (list preproc-source?
                                                                                      markup-source?
                                                                                      markdown-source?
                                                                                      pagetree-source?))])
                                                                (proc path)))
                                                path))
  
  ;; if a file is already in the cache, no need to hit it again.
  ;; this allows partially completed preheat jobs to resume.
  (define uncached-paths (filter
                          (λ (path)
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
                     (message (format "compiling: ~a" path))
                     ;; use #f to signal compile error. Otherwise allow errors to pass.
                     (define result (with-handlers ([exn:fail? (λ _ (message (format "compile failed: ~a" path-name)) #f)])
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