#lang racket/base
(require racket/file
         racket/path
         racket/place
         racket/list
         sugar/list
         "file-utils.rkt"
         "cache-utils.rkt"
         "debug.rkt")
(provide preheat-cache)

(define (path-cached? path)
  ;; #true = already cached; #false = not cached
  ;; seems like it would be slow to load cache.rktd but it's not.
  (define-values (_ private-cache-dir) (make-cache-dirs path))
  (define cache-db-file (build-path private-cache-dir "cache.rktd"))
  (or (file-exists? cache-db-file)
      (hash-has-key? (file->value cache-db-file) (paths->key path))))

;; compile a path inside a place (= parallel processing)
(define (path-into-place starting-dir path)
  (message (format "caching: ~a" (find-relative-path starting-dir path)))
  (define p
    (place ch
           (define path (place-channel-get ch))
           (define-values (_ path-name __) (split-path path))
           (message (format "compiling: ~a" path))
           ;; use #false to signal compile error. Otherwise allow errors to pass.
           (define result
             (with-handlers ([exn:fail? (λ (e) (message (format "compile failed: ~a" path-name)) #false)])
               (path->hash path)))
           (place-channel-put ch result)))
  (place-channel-put p path)
  p)

(define (preheat-cache starting-dir)
  (unless (and (path-string? starting-dir) (directory-exists? starting-dir))
    (raise-argument-error 'preheat-cache "directory" starting-dir))
  (define max-places (processor-count)) ; number of parallel processes to spawn at a time
  (define paths-that-should-be-cached
    (for/list ([path (in-directory starting-dir)]
               #:when (for/or ([proc (in-list (list preproc-source?
                                                    markup-source?
                                                    markdown-source?
                                                    pagetree-source?))])
                              (proc path)))
              path))

  ;; if a file is already in the cache, no need to hit it again.
  ;; this allows partially completed preheat jobs to resume.
  (define uncached-paths (filter-not path-cached? paths-that-should-be-cached))
  
  ;; compile the paths in groups, so they can be incrementally saved.
  ;; that way, if there's a failure, the progress is preserved.
  ;; but the slowest file in a group will prevent further progress.
  (for ([path-group (in-list (slice-at uncached-paths max-places))])
       (define path-places (map (λ (pg) (path-into-place starting-dir pg)) path-group))
       (for ([path (in-list path-group)]
             [ppl (in-list path-places)])
            (define result (place-channel-get ppl))
            (when result ; #false is used to signal compile error
              (cache-ref! (paths->key path) (λ () result))))))