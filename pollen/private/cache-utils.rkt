#lang racket/base
(require "file-utils.rkt"
         "../setup.rkt"
         "project.rkt"
         "log.rkt"
         file/cache
         racket/file
         racket/path
         racket/list
         racket/port
         racket/match
         sugar/coerce
         sugar/test
         racket/fasl
         racket/serialize
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
(define (paths->key cache-type source-path [template-path #false] [output-path #false])
  (unless (symbol? cache-type)
    (raise-argument-error 'paths->key "symbol" cache-type))
  (define path-strings-to-track
    (list* source-path
           ;; if template has a source file, track that instead
           (and template-path (or (get-source template-path) template-path))
           ;; is either list of files or (list #f)
           (append (->list (get-directory-require-files source-path))
                   ;; user-designated files to track
                   (map ->string (setup:cache-watchlist source-path)))))
  (define env-rec (for/list ([env-name (in-list (cons default-env-name (sort (setup:envvar-watchlist source-path) bytes<?)))])
                            (cons env-name (match (getenv (->string env-name))
                                             [#false #false]
                                             [str (string-downcase (->string str))]))))
  (define poly-flag (and (has-inner-poly-ext? source-path) (current-poly-target)))
  (define path+mod-time-pairs
    (for/list ([ps (in-list path-strings-to-track)])
              (match ps
                [(? symbol? sym) sym]
                [#false #false]
                [_ (define cp (->complete-path ps))
                   (unless (file-exists? cp)
                     (message (format "watchlist file /~a does not exist" (find-relative-path (current-project-root) cp))))
                   (cons (path->string cp) (file-or-directory-modify-seconds cp #false (λ () 0)))])))
  (list* cache-type env-rec poly-flag (and output-path (path->string output-path)) path+mod-time-pairs))

(define (key->source-path key) (car (fifth key)))
(define (key->output-path key) (fourth key))
(define (key->type key) (car key))

(module-test-internal
 (define ps "/users/nobody/project/source.html.pm")
 (check-equal? (key->source-path (paths->key 'source ps)) ps))

(define-namespace-anchor cache-utils-module-ns)

;; faster than the usual `managed-compile-zo`
(define caching-zo-compiler (make-caching-managed-compile-zo))

(define (path->hash path)
  (define compilation-namespace
    (cond
      [(current-session-interactive?)
       ;; in interactive mode, we need a fresh namespace every time
       ;; and can't use bytecode, because it's possible that path
       ;; or any dependency (say, "pollen.rkt") has changed
       (define bns (make-base-namespace))
       (define outer-ns (namespace-anchor->namespace cache-utils-module-ns))
       ;; bring in currently instantiated params (unlike namespace-require)
       (namespace-attach-module outer-ns 'pollen/setup bns)
       bns]
      [else
       ;; make bytecode, because we know that in a non-interactive sesssion
       ;; the sources won't change in the midst
       (for-each caching-zo-compiler (cons path (or (get-directory-require-files path) null)))
       ; recycle namespace
       (current-namespace)]))
  ;; I monkeyed around with using the metas submodule to pull out the metas (for speed)
  ;; but in practice most files get their doc requested too.
  ;; so it's just simpler to get both at once and be done with it.
  ;; the savings of avoiding two cache fetches at the outset outweighs
  ;; the benefit of not reloading doc when you just need metas.
  ;; new namespace forces `dynamic-require` to re-instantiate `path`
  ;; otherwise it gets cached in current namespace.
  (define doc-missing-thunk (λ () ""))
  (define metas-missing-thunk (λ () (hasheq)))
  (parameterize ([current-namespace compilation-namespace]
                 [current-directory (dirname path)])
    (hasheq pollen-main-export (dynamic-require path pollen-main-export doc-missing-thunk)
            pollen-meta-export (dynamic-require path pollen-meta-export metas-missing-thunk))))

(define (my-make-directory* dir)
  (define base (dirname dir))
  (when (and (path? base) (not (directory-exists? base)))
    (my-make-directory* base))
  (unless (directory-exists? dir)
    (with-handlers ([exn:fail:filesystem:exists? void])
      (make-directory dir))))

(define (make-cache-dirs path)
  (define path-dir (dirname path))
  (define cache-dir (build-path path-dir pollen-cache-dir-name pollen-cache-subdir-name))
  (define private-cache-dir (build-path cache-dir "private"))
  (my-make-directory* private-cache-dir) ; will also make cache-dir, if needed
  (values cache-dir private-cache-dir))

(define (cache-ref! key path-hash-thunk
                    #:notify-cache-use [notify-proc void])
  (define dest-path ((match (key->type key)
                       ['source key->source-path]
                       ['output key->output-path]
                       ;; path-add-suffix is deprecated since 6.5.0.3 but we still need compatibility with 6.3
                       ['template (λ (k) (path-add-suffix  (key->source-path key) (string->bytes/utf-8 (format ".~a-template" (current-poly-target)))))]) key))
  (define-values (cache-dir private-cache-dir) (make-cache-dirs dest-path))
  (define-values (dest-path-dir dest-path-filename _) (split-path dest-path))
  (define dest-file (build-path cache-dir (format "~a.rktd" dest-path-filename)))
  (define (generate-dest-file)
    (message-debug (format "cache miss for ~a" dest-file))
    #;(with-output-to-file dest-file
        (λ ()
          (define op (open-output-bytes))
          (s-exp->fasl (path-hash-thunk) op)
          (write-bytes (get-output-bytes op)))
        #:exists 'replace)
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
              #:max-cache-files +inf.0
              #:max-cache-size (setup:compile-cache-max-size)
              #:log-debug-string message-debug
              #:log-error-string
              (λ (str)
                (match str
                  ;; concurrency-related error that has no larger consequence
                  [(or "cache attempt failed: could not acquire exclusive lock"
                       "cache attempt failed: could not acquire shared lock") (void)]
                  [_ (log-pollen-error str)])))
  #;(with-input-from-file dest-file
      (λ () (fasl->s-exp (port->bytes))))
  (deserialize (file->value dest-file)))
