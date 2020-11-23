#lang racket/base
(require racket/file
         racket/list
         racket/path
         sugar/define
         "private/cache-utils.rkt"
         "private/log.rkt"
         "setup.rkt")

;; The cache is a hash with paths as keys.
;; The cache values are also hashes, with key/value pairs for that path.

(define (cache-directory? path)
  (and (directory-exists? path)
       (member (path->string (last (explode-path path))) default-cache-names)))

(define+provide (reset-cache [starting-dir (current-project-root)])
  (unless (and (path-string? starting-dir) (directory-exists? starting-dir))
    (raise-argument-error 'reset-cache "path-string to existing directory" starting-dir))
  (for ([path (in-directory starting-dir)]
        #:when (cache-directory? path))
    (message (format "removing cache directory: ~a" path))
    (delete-directory/files path)))

(define ((path-error-handler caller-name path-or-path-string) e)
  (raise-argument-error caller-name "valid path or path-string" path-or-path-string))

(define-namespace-anchor cache-module-ns)

(define (fetch-val path subkey)
  (parameterize ([current-namespace (make-base-namespace)])
    ;; brings in currently instantiated params (unlike namespace-require)
    (define outer-ns (namespace-anchor->namespace cache-module-ns))
    (namespace-attach-module outer-ns 'pollen/setup) 
    (dynamic-require path subkey)))

(define cached-require-base
  (let ([ram-cache (make-hash)])
    (λ (path-or-path-string subkey caller-name)
      (define path
        (with-handlers ([exn:fail? (path-error-handler caller-name path-or-path-string)])
          (simple-form-path (if (path? path-or-path-string)
                                   path-or-path-string
                                   (string->path path-or-path-string)))))
      (unless (file-exists? path)
        (raise-argument-error caller-name "path to existing file" path-or-path-string))
      (cond
        [(setup:compile-cache-active path)
         (define key (paths->key 'source path))
         (define (convert-path-to-cache-record)
           (when (let ([crs (current-render-source)])
                   (and crs (not (equal? crs path))))
             (message (format "transitively loading /~a" (find-relative-path (current-project-root) path))))
           (path->hash path))
         (define (get-cache-record) (cache-ref! key convert-path-to-cache-record))
         (define ram-cache-record (hash-ref! ram-cache key get-cache-record))
         (hash-ref ram-cache-record subkey)]
        [else (fetch-val path subkey)]))))

(define+provide (cached-require path-string subkey)
  (cached-require-base path-string subkey 'cached-require))

(define+provide (cached-doc path-string)
  (cached-require-base path-string pollen-main-export 'cached-doc))

(define+provide (cached-metas path-string)
  (cached-require-base path-string pollen-meta-export 'cached-metas))