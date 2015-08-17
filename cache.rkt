#lang racket/base
(require racket/path racket/file file/cache sugar/coerce "project.rkt" "world.rkt" "rerequire.rkt")

;; The cache is a hash with paths as keys.
;; The cache values are also hashes, with key/value pairs for that path.

(provide reset-cache cached-require paths->key)


(define cache-dir
  (build-path (world:current-project-root) (world:current-cache-dir-name)))


(define (reset-cache)
  (cache-remove #f cache-dir))


(define (paths->key source-path [template-path #f])
  ;; key is list of file + mod-time pairs
  (define path-strings (append (list source-path)
                        (if template-path (list template-path) null)
                        (or (get-directory-require-files source-path) null)))
  (define project-root (world:current-project-root))
  ;; can't use relative paths for cache keys because source files include `here-path` which is absolute.
  ;; problem is that cache could appear valid on another filesystem (based on relative pathnames & mod dates)
  ;; but would actually be invalid (because the `here-path` names are wrong).
  (map (λ(ps) (define cp (->complete-path ps))
         (cons (path->string cp) (file-or-directory-modify-seconds cp))) path-strings))

(define (key->source-path key)
  (car (car key)))

(define (update-directory-requires source-path)
  (define directory-require-files (get-directory-require-files source-path))
  (and directory-require-files (map dynamic-rerequire directory-require-files))
  (void))


(define (path->hash path)
  (dynamic-rerequire path)
  ;; new namespace forces dynamic-require to re-instantiate 'path'
  ;; otherwise it gets cached in current namespace.
  (parameterize ([current-namespace (make-base-namespace)])
    (hash (world:current-main-export) (dynamic-require path (world:current-main-export))
          (world:current-meta-export) (dynamic-require path (world:current-meta-export)))))


(define ram-cache (make-hash))


(require sugar/debug)
(define (cached-require path-string subkey)  
  (define path (with-handlers ([exn:fail? (λ _ (error 'cached-require (format "~a is not a valid path" path-string)))])
                 (->complete-path path-string)))
  
  (when (not (file-exists? path))
    (error (format "cached-require: ~a does not exist" path)))
  
  (cond
    [(world:current-compile-cache-active)
     (define key (paths->key path))
     ;; use multiple pickup files to avoid locking issues.
     ;; pickup-file hierarchy just mirrors the project hierarchy.
     (define dest-file (build-path cache-dir (path->string (find-relative-path (world:current-project-root) (string->path (format "~a.rktd" (key->source-path key)))))))
     (make-parent-directory* dest-file)
     (hash-ref (hash-ref! ram-cache key (λ _
                                          (cache-file dest-file
                                                      #:exists-ok? #t
                                                      key
                                                      cache-dir
                                                      (λ _ (write-to-file (path->hash path) dest-file #:exists 'replace))
                                                      #:max-cache-size (world:current-compile-cache-max-size))
                                          (file->value dest-file))) subkey)]
    [else (parameterize ([current-namespace (make-base-namespace)])
            (dynamic-require path subkey))]))