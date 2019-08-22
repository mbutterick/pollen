#lang racket/base
(require "file-utils.rkt"
         "../setup.rkt"
         "project.rkt"
         "log.rkt"
         file/cache
         racket/file
         racket/path
         racket/list
         racket/string
         racket/serialize
         sugar/coerce
         sugar/test
         compiler/cm)
(provide (all-defined-out))

;; can't use relative paths for cache keys because source files include `here-path` which is absolute.
;; problem is that cache could appear valid on another filesystem (based on relative pathnames & mod dates)
;; but would actually be invalid (because the `here-path` names are wrong).
;; key is list of file + mod-time pairs, use #f for missing  
;; we don't include output-path in path-strings-to-track
;; because we don't want to attach a mod date
;; because cache validity is not sensitive to mod date of output path
;; (in fact we would expect it to be earlier, since we want to rely on an earlier version)
(define (paths->key source-path [template-path #false] [output-path #false])
  (define-values (env-watchlist path-watchlist) (partition bytes? (setup:cache-watchlist source-path)))
  (define path-strings-to-track
    (list* source-path
           ;; if template has a source file, track that instead
           (and template-path (or (get-source template-path) template-path))
           ;; is either list of files or (list #f)
           (append (->list (get-directory-require-files source-path))
                   ;; user-designated files to track
                   (map ->string path-watchlist))))
  (define pollen-env (getenv default-env-name))
  (define env-rec (for/list ([env-name (in-list (sort env-watchlist bytes<?))])
                            (cons env-name (environment-variables-ref (current-environment-variables) env-name))))
  (define poly-flag (and (has-inner-poly-ext? source-path) (current-poly-target)))
  (define path+mod-time-pairs
    (for/list ([ps (in-list path-strings-to-track)])
              (cond
                [ps (define cp (->complete-path ps))
                    (unless (file-exists? cp)
                      (message (format "watchlist file /~a does not exist" (find-relative-path (current-project-root) cp))))
                    (cons (path->string cp) (file-or-directory-modify-seconds cp #false (λ () 0)))]
                [else #false])))
  (append env-rec (list pollen-env poly-flag (and output-path (path->string output-path))) path+mod-time-pairs))

(define (key->source-path key) (car (fourth key)))
(define (key->output-path key) (third key))

(module-test-internal
 (define ps "/users/nobody/project/source.html.pm")
 (check-equal? (key->source-path (paths->key ps)) ps))

(define-namespace-anchor cache-utils-module-ns)

(define (path->hash path)
  (for-each managed-compile-zo (or (get-directory-require-files path) null))
  (apply hasheq
         (let ([doc-key (setup:main-export)] [meta-key (setup:meta-export)])
           (unless (and (symbol? doc-key) (symbol? meta-key))
             (raise-argument-error 'path->hash "symbols for doc and meta key" (cons doc-key meta-key)))
           ;; I monkeyed around with using the metas submodule to pull out the metas (for speed)
           ;; but in practice most files get their doc requested too.
           ;; so it's just simpler to get both at once and be done with it.
           ;; the savings of avoiding two cache fetches at the outset outweighs
           ;; the benefit of not reloading doc when you just need metas.
           ;; new namespace forces `dynamic-require` to re-instantiate `path`
           ;; otherwise it gets cached in current namespace.
           (parameterize ([current-namespace (make-base-namespace)]
                          [current-directory (dirname path)])
             ;; brings in currently instantiated params (unlike namespace-require)
             (define outer-ns (namespace-anchor->namespace cache-utils-module-ns))
             (namespace-attach-module outer-ns 'pollen/setup) 
             (define doc-missing-thunk (λ () ""))
             (define metas-missing-thunk (λ () (hasheq)))
             (list doc-key (dynamic-require path doc-key doc-missing-thunk)
                   meta-key (dynamic-require path meta-key metas-missing-thunk))))))

(define (my-make-directory* dir)
  (define base (dirname dir))
  (when (and (path? base) (not (directory-exists? base)))
    (my-make-directory* base))
  (unless (directory-exists? dir)
    (with-handlers ([exn:fail:filesystem:exists? void])
      (make-directory dir))))

(define (make-cache-dirs path)
  (define path-dir (dirname path))
  (define cache-dir (build-path path-dir (setup:cache-dir-name) (setup:cache-subdir-name)))
  (define private-cache-dir (build-path cache-dir "private"))
  (my-make-directory* private-cache-dir) ; will also make cache-dir, if needed
  (values cache-dir private-cache-dir))

(define (cache-ref! key path-hash-thunk
                    #:dest-path [path-for-dest 'source]
                    #:notify-cache-use [notify-proc void])
  (define dest-path ((case path-for-dest
                       [(source) key->source-path]
                       [(output) key->output-path]) key))
  (define-values (cache-dir private-cache-dir) (make-cache-dirs dest-path))
  (define-values (dest-path-dir dest-path-filename _) (split-path dest-path))
  (define dest-file (build-path cache-dir (format "~a.rktd" dest-path-filename)))
  (define (generate-dest-file)
    (write-to-file (serialize (path-hash-thunk)) dest-file #:exists 'replace))

  ;; `cache-file` looks for a file in private-cache-dir previously cached with key
  ;; (which in this case carries modification dates and POLLEN env).
  ;; If a cached file is found, copies it to dest-file (which must not exist already, unless exists-ok? is true)
  ;; Otherwise, generate-dest-file is called; if dest-file exists after calling fetch-dest-file,
  ;; it is copied to private-cache-dir and recorded with key.
  (cache-file dest-file
              #:exists-ok? #true
              key
              private-cache-dir
              generate-dest-file
              #:notify-cache-use notify-proc
              #:max-cache-size (setup:compile-cache-max-size))
  (deserialize (file->value dest-file)))