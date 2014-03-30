#lang racket/base
(require racket/rerequire "world.rkt")

;; The cache is a hash with paths as keys.
;; The cache values are also hashes, with key/value pairs for that path.

(provide reset-cache current-cache make-cache cached-require cache-ref)

(define (make-cache) (make-hash))

(define current-cache (make-parameter (make-cache)))

(define (reset-cache) (hash-clear! (current-cache)))

(define (->complete-path path-string)
  (path->complete-path (if (string? path-string) (string->path path-string) path-string)))

(define (cache-ref path-string)
  (hash-ref (current-cache) (->complete-path path-string)))

(define (cache-has-key? path)
  (hash-has-key? (current-cache) path))

(define (cache path)  
  (dynamic-rerequire path)
  (hash-set! (current-cache) path (make-hash))
  (define cache-hash (cache-ref path))
  (hash-set! cache-hash 'mod-time (file-or-directory-modify-seconds path))
  (hash-set! cache-hash world:main-pollen-export (dynamic-require path world:main-pollen-export))
  (hash-set! cache-hash world:meta-pollen-export (dynamic-require path world:meta-pollen-export))
  (void))

(define (cached-require path-string key)
  (when (not (current-cache)) (error "cached-require: No cache set up."))
  
  (define path 
    (with-handlers ([exn:fail? (λ(exn) (error (format "cached-require: ~a is not a valid path" path-string)))])
      (->complete-path path-string)))  
  
  (when (not (file-exists? path)) (error (format "cached-require: ~a does not exist" (path->string path))))
  
  (when (or (not (cache-has-key? path))
            (> (file-or-directory-modify-seconds path) (hash-ref (cache-ref path) 'mod-time)))
    (cache path))
  
  (hash-ref (cache-ref path) key))



