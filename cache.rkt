#lang racket/base
(require racket/path racket/file compiler/cm file/cache sugar/coerce "project.rkt" "world.rkt" "rerequire.rkt" "cache-ns.rkt" "debug.rkt")

;; The cache is a hash with paths as keys.
;; The cache values are also hashes, with key/value pairs for that path.

(provide reset-cache cached-require paths->key)


(define (get-cache-dir)
  (build-path (world:current-project-root) (world:current-cache-dir-name)))


(define (reset-cache)
  (for ([path (in-directory)]
        #:when (and (directory-exists? path)
                    (equal? (path->string (car (reverse (explode-path path)))) (world:current-cache-dir-name))))
       (message (format "removing cache directory: ~a" path))
       (delete-directory/files path)))


(define (paths->key source-path [template-path #f])
  ;; key is list of file + mod-time pairs, use #f for missing
  (define path-strings (append (list source-path)
                               (append (list template-path) ; is either path or #f
                                       (->list (get-directory-require-files source-path))))) ; is either list of files or (list #f)
  ;; can't use relative paths for cache keys because source files include `here-path` which is absolute.
  ;; problem is that cache could appear valid on another filesystem (based on relative pathnames & mod dates)
  ;; but would actually be invalid (because the `here-path` names are wrong).
  (define path+mod-time-pairs
    (map (λ(ps) (and ps (let ([cp (->complete-path ps)])
                          (cons (path->string cp) (file-or-directory-modify-seconds cp))))) path-strings))
  path+mod-time-pairs)


(define (key->source-path key)
  (car (car key)))


(define (path->hash path)
  ;; new namespace forces dynamic-require to re-instantiate 'path'
  ;; otherwise it gets cached in current namespace.
  (define drfs (get-directory-require-files path))
  (for-each managed-compile-zo (or drfs null))
  
  (apply hash
         (let ([doc-key (world:current-main-export)]
               [meta-key (world:current-meta-export)])
           (parameterize ([current-namespace (make-base-namespace)])
             ;; I monkeyed around with using the metas submodule to pull out the metas (for speed)
             ;; but in practice most files get their doc requested too.
             ;; so it's just simpler to get both at once and be done with it.
             ;; the savings of avoiding two cache fetches at the outset outweighs
             ;; the benefit of not reloading doc when you just need metas.
             (list doc-key (dynamic-require path doc-key) meta-key (dynamic-require path meta-key))))))

;; include this from 6.2 for compatibility back to 6.0 (formerly `make-parent-directory*`)
(define (my-make-directory* dir)
  (let-values ([(base name dir?) (split-path dir)])
    (when (and (path? base) (not (directory-exists? base)))
      (my-make-directory* base))
    (unless (directory-exists? dir)
      (with-handlers ([exn:fail:filesystem:exists? void])
        (make-directory dir)))))

(define (my-make-parent-directory* p)
  (unless (path-string? p)
    (raise-argument-error 'make-parent-directory "path-string?" p))
  (define-values (base name dir?) (split-path p))
  (when (path? base)
    (my-make-directory* base)))


(define ram-cache (make-hash))

(define (cached-require path-string subkey)  
  (define path (with-handlers ([exn:fail? (λ _ (error 'cached-require (format "~a is not a valid path" path-string)))])
                 (->complete-path path-string)))
  
  (when (not (file-exists? path))
    (error (format "cached-require: ~a does not exist" path)))
  
  (cond
    [(world:current-compile-cache-active)
     (define key (paths->key path))
     (define cache-dir (get-cache-dir))
     (define private-cache-dir (build-path cache-dir "private"))
     ;; cache-dir is also inside current-project-root. So there is a separate pollen-cache in each subdir.
     (define dest-file (build-path cache-dir (format "~a.rktd" (find-relative-path (world:current-project-root) path))))
     (my-make-parent-directory* dest-file)
     (my-make-directory* private-cache-dir)
     (hash-ref (hash-ref! ram-cache key (λ _
                                          (cache-file dest-file
                                                      #:exists-ok? #t
                                                      key
                                                      private-cache-dir
                                                      (λ _
                                                        (write-to-file (path->hash path) dest-file #:exists 'replace))
                                                      #:max-cache-size (world:current-compile-cache-max-size))
                                          (file->value dest-file))) subkey)]
    [else (parameterize ([current-namespace (make-base-namespace)])
            (dynamic-require path subkey))]))