#lang debug racket/base
(require racket/file
         racket/path
         racket/place
         racket/list
         racket/match
         sugar/list
         "file-utils.rkt"
         "cache-utils.rkt"
         "log.rkt")
(provide preheat-cache)

(define (path-cached? path)
  ;; #true = already cached; #false = not cached
  ;; seems like it would be slow to load cache.rktd but it's not.
  (define-values (_ private-cache-dir) (make-cache-dirs path))
  (define cache-db-file (build-path private-cache-dir "cache.rktd"))
  (and (file-exists? cache-db-file)
       (hash-has-key? (file->value cache-db-file) (paths->key path))))

(define (preheat-cache starting-dir)
  (unless (and (path-string? starting-dir) (directory-exists? starting-dir))
    (raise-argument-error 'preheat-cache "directory" starting-dir))
  (define max-places (processor-count)) ; number of parallel processes to spawn at a time
  (define worker-places (for/list ([i (in-range max-places)])
                          (place ch
                                 (let loop ()
                                   (define result (with-handlers ([exn:fail? (λ (e) #false)])
                                                        (path->hash (place-channel-get ch))))
                                   (place-channel-put ch result)
                                   (loop)))))
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
    (for ([path (in-list path-group)]
          [wp (in-list worker-places)])
      (message (format "caching: ~a" (find-relative-path starting-dir path)))
      (place-channel-put wp path))
    (for ([path (in-list path-group)]
          [wp (in-list worker-places)])
      (match (place-channel-get wp)
        [#false (message (format "compile failed: ~a" path))]
        [result (cache-ref! (paths->key path) (λ () result))]))))