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

(define (key->source-path-key key)
  (list (car key)))

(define (key->source-path key)
  (car (car key)))

(define (update-directory-requires source-path)
  (define directory-require-files (get-directory-require-files source-path))
  (and directory-require-files (map dynamic-rerequire directory-require-files))
  (void))

(require sugar/debug)

(define (path->hash path subkey)
  (dynamic-rerequire path)
  ;; new namespace forces dynamic-require to re-instantiate 'path'
  ;; otherwise it gets cached in current namespace.
  (parameterize ([current-namespace (make-base-namespace)])
    (hash subkey (dynamic-require (if (eq? subkey (world:current-meta-export))
                                      `(submod ,path ,subkey) ; use metas submodule for speed
                                      path) subkey))))

;; include this from 6.2 for compatibility back to 6.0 (formerly `make-parent-directory*`)
(define (make-parent-directory p)
  (unless (path-string? p)
    (raise-argument-error 'make-parent-directory "path-string?" p))
  
  (define (make-directory* dir)
    (let-values ([(base name dir?) (split-path dir)])
      (when (and (path? base) (not (directory-exists? base)))
        (make-directory* base))
      (unless (directory-exists? dir)
        (with-handlers ([exn:fail:filesystem:exists? void])
          (make-directory dir)))))
  
  (define-values (base name dir?) (split-path p))
  (when (path? base)
    (make-directory* base)))


(define ram-cache (make-hash))

(define (cached-require path-string subkey)  
  (define path (with-handlers ([exn:fail? (λ _ (error 'cached-require (format "~a is not a valid path" path-string)))])
                 (->complete-path path-string)))
  
  (when (not (file-exists? path))
    (error (format "cached-require: ~a does not exist" path)))
  
  (cond
    [(world:current-compile-cache-active)
     (define key (let ([possible-key (paths->key path)])
                   (if (eq? subkey (world:current-meta-export))
                       (key->source-path-key possible-key)
                       possible-key)))
     ;; use multiple pickup files to avoid locking issues.
     ;; pickup-file hierarchy just mirrors the project hierarchy.
     (define dest-file (build-path cache-dir (path->string (find-relative-path (world:current-project-root) (string->path (format "~a#~a.rktd" (key->source-path key) subkey))))))
     (make-parent-directory dest-file)
     (hash-ref (hash-ref! ram-cache key (λ _
                                          (cache-file dest-file
                                                      #:exists-ok? #t
                                                      key
                                                      cache-dir
                                                      (λ _ (write-to-file (path->hash path subkey) dest-file #:exists 'replace))
                                                      #:max-cache-size (world:current-compile-cache-max-size))
                                          (file->value dest-file))) subkey)]
    [else (parameterize ([current-namespace (make-base-namespace)])
            (dynamic-require path subkey))]))