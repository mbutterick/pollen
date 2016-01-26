#lang racket/base
(require racket/file
         sugar/define
         "private/cache-utils.rkt"
         "private/debug.rkt"
         "world.rkt")

;; The cache is a hash with paths as keys.
;; The cache values are also hashes, with key/value pairs for that path.

(define+provide (reset-cache [starting-dir (world:current-project-root)])
  (unless (and (path-string? starting-dir) (directory-exists? starting-dir))
    (raise-argument-error 'reset-cache "path-string to existing directory" starting-dir))
  
  (for ([path (in-directory starting-dir)]
        #:when (and (directory-exists? path)
                    (equal? (path->string (car (reverse (explode-path path)))) (world:current-cache-dir-name))))
       (message (format "removing cache directory: ~a" path))
       (delete-directory/files path)))

(define-namespace-anchor cache-module-ns)
(define cached-require-base
  (let ([ram-cache (make-hash)])
    (λ(path-or-path-string subkey caller-name)
      (define path (with-handlers ([exn:fail? (λ(e) (raise-argument-error caller-name "valid path or path-string" path-or-path-string))])
                     (path->complete-path (if (path? path-or-path-string)
                                              path-or-path-string
                                              (string->path path-or-path-string)))))
      
      (unless (file-exists? path)
        (raise-argument-error caller-name "path to existing file" path-or-path-string))
      
      (cond
        [(world:current-compile-cache-active path)
         (define key (paths->key path))
         (hash-ref (hash-ref! ram-cache key (λ _ (cache-ref! key (λ _ (path->hash path))))) subkey)]
        [else (parameterize ([current-namespace (make-base-namespace)])
                (namespace-attach-module (namespace-anchor->namespace cache-module-ns) 'pollen/world) ; brings in params
                (dynamic-require path subkey))]))))


(define+provide (cached-require path-string subkey)
  (cached-require-base path-string subkey 'cached-require))


(define+provide (cached-doc path-string)
  (cached-require-base path-string (world:current-main-export) 'cached-doc))


(define+provide (cached-metas path-string)
  (cached-require-base path-string (world:current-meta-export) 'cached-metas))