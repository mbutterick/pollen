#lang racket/base
(require racket/rerequire)
(require "debug.rkt" sugar/coercion/values)

;; The cache is a hash with paths as keys.
;; The cache values are also hashes, with key/value pairs for that path.

(provide current-cache make-cache cached-require cache-ref)

;; Don't initialize a cache when the module is loaded. This induces reliance.
;; The cache only makes sense if a single one is used across a whole session (e.g., via parameterize).
(define current-cache (make-parameter #f))

(define (make-cache) (make-hash))

(define (cache-ref path-string)
  (hash-ref (current-cache) (->complete-path path-string)))

(define (cache-has-key? path)
  (hash-has-key? (current-cache) path))

(define (cache path)
  (dynamic-rerequire path)
  (hash-set! (current-cache) path (make-hash))
  (define cache-hash (cache-ref path))
  (hash-set! cache-hash 'mod-time (file-or-directory-modify-seconds path))
  (hash-set! cache-hash 'main (dynamic-require path 'main))
  (hash-set! cache-hash 'metas (dynamic-require path 'metas))
  (void))


(define (cached-require path-string key)
  (when (not (current-cache)) (error "cached-require: No cache set up."))
  
  (define path 
    (with-handlers ([exn:fail? (λ(exn) (displayln (format "cached-require: ~a is not a valid path" path-string)))])
      (->complete-path path-string)))  
  
  (when (or (not (cache-has-key? path))
            (> (file-or-directory-modify-seconds path) (hash-ref (cache-ref path) 'mod-time)))
    (cache path))
  
  (hash-ref (cache-ref path) key))



