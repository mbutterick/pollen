#lang racket/base
(require "file-utils.rkt" "../setup.rkt" "project.rkt" file/cache racket/file sugar/coerce compiler/cm)
(provide (all-defined-out))

(define (paths->key source-path [template-path #f])
  ;; key is list of file + mod-time pairs, use #f for missing
  (define path-strings (append (list source-path)
                               ;; if template has a source file, track that instead
                               (append (list (and template-path (or (get-source template-path) template-path)))
                                       ;; is either list of files or (list #f)
                                       (->list (get-directory-require-files source-path)))))
  ;; can't use relative paths for cache keys because source files include `here-path` which is absolute.
  ;; problem is that cache could appear valid on another filesystem (based on relative pathnames & mod dates)
  ;; but would actually be invalid (because the `here-path` names are wrong).
  (define poly-flag (and (has-inner-poly-ext? source-path) (current-poly-target)))
  (define pollen-env (getenv default-env-name))
  (define path+mod-time-pairs
    (map (λ (ps) (and ps (let ([cp (->complete-path ps)])
                          (cons (path->string cp) (with-handlers ([exn:fail? (λ _ 0)])
                                                    (file-or-directory-modify-seconds cp)))))) path-strings))
  (list* pollen-env poly-flag path+mod-time-pairs))


(define (key->source-path key)
  (car (caddr key)))

(require sugar/test)
(module-test-internal
 (define ps "/users/nobody/project/source.html.pm")
 (check-equal? (key->source-path (paths->key ps)) ps))


(define-namespace-anchor cache-utils-module-ns)
(define (path->hash path)
  ;; new namespace forces dynamic-require to re-instantiate 'path'
  ;; otherwise it gets cached in current namespace.
  (define drfs (get-directory-require-files path))
  (for-each managed-compile-zo (or drfs null))
  (define path-dir (dirname path))
  (apply hash
         (let ([doc-key (setup:main-export)]
               [meta-key (setup:meta-export)])
           (parameterize ([current-namespace (make-base-namespace)]
                          [current-directory path-dir])
             ;; I monkeyed around with using the metas submodule to pull out the metas (for speed)
             ;; but in practice most files get their doc requested too.
             ;; so it's just simpler to get both at once and be done with it.
             ;; the savings of avoiding two cache fetches at the outset outweighs
             ;; the benefit of not reloading doc when you just need metas.
             (namespace-attach-module (namespace-anchor->namespace cache-utils-module-ns) 'pollen/setup) ; brings in params
             (define doc-missing-thunk (λ () ""))
             (define metas-missing-thunk (λ () (hasheq)))
             (list doc-key (dynamic-require path doc-key doc-missing-thunk)
                   meta-key (dynamic-require path meta-key metas-missing-thunk))))))

(define (my-make-directory* dir)
  (let-values ([(base name dir?) (split-path dir)])
    (when (and (path? base) (not (directory-exists? base)))
      (my-make-directory* base))
    (unless (directory-exists? dir)
      (with-handlers ([exn:fail:filesystem:exists? void])
        (make-directory dir)))))

(define (make-cache-dirs path)
  (define path-dir (dirname path))
  (define cache-dir (build-path path-dir (setup:cache-dir-name) (setup:cache-subdir-name)))
  (define private-cache-dir (build-path cache-dir "private"))
  (my-make-directory* private-cache-dir) ; will also make cache-dir, if needed
  (values cache-dir private-cache-dir))

(define (cache-ref! key path-hash-thunk)
  (define path (key->source-path key))
  (define-values (cache-dir private-cache-dir) (make-cache-dirs path))
  (define-values (path-dir path-filename _) (split-path path))
  (define dest-file (build-path cache-dir (format "~a.rktd" path-filename)))
  (cache-file dest-file
              #:exists-ok? #t
              key
              private-cache-dir
              (λ _
                (write-to-file (path-hash-thunk) dest-file #:exists 'replace))
              #:max-cache-size (setup:compile-cache-max-size))
  (file->value dest-file))