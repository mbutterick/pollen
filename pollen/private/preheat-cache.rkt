#lang racket/base
(require racket/file
         racket/path
         racket/place
         racket/match
         racket/format
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
       (hash-has-key? (file->value cache-db-file) (paths->key 'source path))))

(define (preheat-cache starting-dir [wants-parallel-setup? #false] [wants-dry-run? #false])
  (unless (and (path-string? starting-dir) (directory-exists? starting-dir))
    (raise-argument-error 'preheat-cache "directory" starting-dir))
  
  ;; if a file is already in the cache, no need to hit it again.
  ;; this allows partially completed preheat jobs to resume.
  (define uncached-paths
    (for/list ([path (in-directory starting-dir (λ (p) (not (special-path? p))))]
               #:when (for/or ([proc (in-list (list preproc-source?
                                                    markup-source?
                                                    markdown-source?
                                                    pagetree-source?))])
                              (proc path))
               #:unless (path-cached? path))
              (simple-form-path path)))

  (cond
    [wants-dry-run? (for-each message uncached-paths)]
    [(null? uncached-paths) (message "all cached files are up to date")]
    [wants-parallel-setup?
     
     (define job-count
       (min
        (length uncached-paths)
        (match wants-parallel-setup?
          [#true (processor-count)]
          [(? exact-positive-integer? count) count]
          [_ (raise-argument-error 'preheat-cache "exact positive integer" wants-parallel-setup?)])))    

     (define worker-evts
       (for/list ([wpidx (in-range job-count)])
                 (define wp
                   (place ch
                          (let loop ()
                            (define path (place-channel-put/get ch (list 'want-job)))
                            (place-channel-put ch (list 'job-finished path 
                                                        (with-handlers ([exn:fail? (λ (e) #f)])
                                                          (path->hash path))))
                            (loop))))
                 (handle-evt wp (λ (val) (list* wpidx wp val)))))
  
     (let loop ([paths uncached-paths][actives null])
       (unless (and (null? paths) (null? actives))
         (match (apply sync worker-evts)
           [(list wpidx wp 'want-job)
            (match paths
              [(? null?) (loop null actives)]
              [(cons path rest)
               (place-channel-put wp path)
               (message (format "caching @ job ~a: ~a" (~r (add1 wpidx) #:min-width (string-length (~r job-count)) #:pad-string " ") (find-relative-path starting-dir path)))
               (loop rest (cons wpidx actives))])]
           [(list wpidx wp 'job-finished path result)
            (if result
                (cache-ref! (paths->key 'source path) (λ () result))
                (message (format "caching failed on job ~a: ~a" (add1 wpidx) (find-relative-path starting-dir path))))
            (loop paths (remq wpidx actives))])))]
    [else (for ([path (in-list uncached-paths)])
               (message (format "caching: ~a" (find-relative-path starting-dir path)))
               (match (with-handlers ([exn:fail? (λ (e) #f)]) (path->hash path))
                 [#false (message (format "caching failed: ~a" (find-relative-path starting-dir path)))]
                 [result (cache-ref! (paths->key 'source path) (λ () result))]))]))