#lang racket/base
(require racket/rerequire racket/contract)
(require "debug.rkt" sugar/coerce)

(provide current-cache make-cache cached-require get-cache-hash)

(define current-cache (make-parameter #f))

(define/contract (path-string->path path-string)
  (path-string? . -> . complete-path?)
  (->complete-path (if (string? path-string) (string->path path-string) path-string)))

(define/contract (make-cache)
  ( ->  hash?)
  (make-hash))

(define/contract (get-cache-hash path-string)
  (path-string? . -> . hash?)
  (hash-ref (current-cache) (path-string->path path-string)))

(define/contract (cache-ref path sym)
  (path? symbol? . -> . any/c)
  (hash-ref (get-cache-hash path) sym))

(define/contract (cached-require path-string sym)
  (path-string? symbol? . -> . any/c)
  (when (not (current-cache)) (error "cached-require: No cache set up."))
  
  (define path (path-string->path path-string))
  
  (define (cache path)
    (dynamic-rerequire path)
    (hash-set! (current-cache) path (make-hash))
    (define cache-hash (hash-ref (current-cache) path))
    (hash-set! cache-hash 'mod-time (file-or-directory-modify-seconds path))
    (hash-set! cache-hash 'main (dynamic-require path 'main))
    (hash-set! cache-hash 'metas (dynamic-require path 'metas))
    (void))
  
  (when (or (not (hash-has-key? (current-cache) path))
            (> (file-or-directory-modify-seconds path) (hash-ref (hash-ref (current-cache) path) 'mod-time)))
    (cache path))
  
  (cache-ref path sym))



