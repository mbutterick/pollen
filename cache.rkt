#lang racket/base
(require racket/path racket/function racket/file file/cache sugar/coerce "project.rkt"  "world.rkt" racket/rerequire "debug.rkt")

;; The cache is a hash with paths as keys.
;; The cache values are also hashes, with key/value pairs for that path.

(provide reset-cache cached-require path->key path->hash)
(provide (all-from-out racket/rerequire))

(define (get-cache-dir)
  (build-path (world:current-project-root) (world:current-cache-dir-name)))


(define (reset-cache)
  (cache-remove #f (get-cache-dir)))


(define (path->key source-path [template-path #f])
  ;; key is list of file + mod-time pairs
  (define path-strings (map (compose1 ->string ->complete-path)
                     (append (list source-path)
                           (if template-path (list template-path) null)
                           (or (get-directory-require-files source-path) null))))
  (map cons path-strings (map file-or-directory-modify-seconds path-strings)))


(define (path->hash path)
  (dynamic-rerequire path)
  (hash (world:current-main-export) (dynamic-require path (world:current-main-export))
         (world:current-meta-export) (dynamic-require path (world:current-meta-export))))


(define (cached-require path-string subkey)
  (define path (with-handlers ([exn:fail? (λ _ (error 'cached-require (format "~a is not a valid path" path-string)))])
                 (->complete-path path-string)))
  
  (when (not (file-exists? path))
    (error (format "cached-require: ~a does not exist" path)))
  
  (cond
    [(world:current-compile-cache-active)
     (define pickup-file (build-path (get-cache-dir) "pickup.rktd"))
     (cache-file pickup-file #:exists-ok? #t
                 (path->key path)
                 (get-cache-dir)
                 (λ _ (write-to-file (path->hash path) pickup-file #:exists 'replace))
                 #:max-cache-size (world:current-compile-cache-max-size))
     (hash-ref (file->value pickup-file) subkey)]
    [else ; cache inactive
     (dynamic-require path subkey)]))
