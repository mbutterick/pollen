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
  (map (λ(ps) (define cp (->complete-path ps))
         (cons (path->string (find-relative-path project-root cp)) (file-or-directory-modify-seconds cp))) path-strings))



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

(define (cached-require path-string subkey)  
  (define path (with-handlers ([exn:fail? (λ _ (error 'cached-require (format "~a is not a valid path" path-string)))])
                 (->complete-path path-string)))
  
  (when (not (file-exists? path))
    (error (format "cached-require: ~a does not exist" path)))
  
  (cond
    [(world:current-compile-cache-active)
     (define key (paths->key path))
     (define pickup-file (build-path cache-dir "pickup.rktd"))
     (hash-ref (hash-ref! ram-cache key (λ _
                                          (cache-file pickup-file
                                                      #:exists-ok? #t
                                                      key
                                                      cache-dir
                                                      (λ _ (write-to-file (path->hash path) pickup-file #:exists 'replace))
                                                      #:max-cache-size (world:current-compile-cache-max-size))
                                          (file->value pickup-file))) subkey)]
    [else (parameterize ([current-namespace (make-base-namespace)])
            (dynamic-require path subkey))]))