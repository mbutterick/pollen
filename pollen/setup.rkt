#lang racket/base
(require (for-syntax racket/base racket/syntax)
         racket/runtime-path
         racket/path
         "private/constants.rkt")
(provide (all-from-out "private/constants.rkt"))

(define-syntax-rule (define+provide ID EXPR ...)
  (begin
    (provide ID)
    (define ID EXPR ...)))

(define+provide current-project-root (make-parameter (current-directory)))

(define+provide default-directory-require "pollen.rkt")
(define+provide default-env-name "POLLEN")

(define (dirname path)
  (let-values ([(dir name dir?) (split-path path)])
    dir))

(define (get-path-to-override maybe-dir)
  (define starting-dir (cond
                         [(not maybe-dir) (current-directory)]
                         [(directory-exists? maybe-dir) maybe-dir]
                         [else (define dir (dirname maybe-dir))
                               (and (not (eq? 'relative dir)) (simplify-path (path->complete-path dir (current-directory))))]))
  (let loop ([dir starting-dir][path default-directory-require])
    (and dir ; dir is #f when it hits the top of the filesystem
         (let ([simplified-path (simplify-path (path->complete-path path starting-dir))])
           (if (file-exists? simplified-path)
               simplified-path
               (loop (dirname dir) (build-path 'up path)))))))


;; parameters should not be made settable.
(define-for-syntax world-submodule-name 'setup)
(define-syntax (define-settable stx)
  (syntax-case stx ()
    [(_ NAME DEFAULT-VALUE)
     (with-syntax ([DEFAULT-NAME (format-id stx "default-~a" #'NAME)]
                   [NAME-THUNKED (format-id stx "~a" #'NAME)]
                   [WORLD-SUBMOD (format-id stx "~a" world-submodule-name)]
                   [NAME-FAIL-THUNKED (format-id stx "fail-thunk-~a" #'NAME)] )
       #'(begin
           (provide (prefix-out setup: NAME-THUNKED) DEFAULT-NAME)
           (define DEFAULT-NAME DEFAULT-VALUE)
           ;; can take a dir argument that sets start point for (get-path-to-override) search.
           (define (NAME-THUNKED [dir #false])
             ;; exn:fail:contract? is raised if setup submodule doesn't exist
             ;; in which case we use the default value.
             ;; but if something else is amiss, we want to let it bubble up
             (define setup-module-path (get-path-to-override dir))
             (with-handlers ([exn:fail:contract? (λ (exn) DEFAULT-NAME)]
                             [exn? (λ (exn) (raise-user-error 'pollen/setup
                                                              (format "defective `setup` submodule in ~v\n~a" (path->string setup-module-path) (exn-message exn))))])
               (dynamic-require `(submod ,setup-module-path WORLD-SUBMOD)
                                   'NAME
                                   (λ () DEFAULT-NAME))))))]))



(define-settable cache-watchlist null)
(define-settable envvar-watchlist null)

;; these are deliberately not settable because they're just internal signalers, no effect on external interface
(define+provide default-mode-auto 'auto)
(define+provide default-mode-preproc 'pre)
(define+provide default-mode-markup 'markup)
(define+provide default-mode-markdown 'markdown)
(define+provide default-mode-pagetree 'ptree)
(define+provide default-mode-template 'template)

(define+provide default-cache-names (cons pollen-cache-dir-name pollen-old-cache-names))

(define-settable decodable-extensions (list pollen-markup-source-ext pollen-pagetree-source-ext))

(define-settable main-pagetree (format "index.~a" pollen-pagetree-source-ext))
(define-settable pagetree-root-node 'pagetree-root)
(define-settable main-root-node 'root)

(define-settable command-char #\◊)
(define-settable template-command-char #\∂)

;; tags from https://developer.mozilla.org/en-US/docs/Web/HTML/Block-level_elements
(define-settable block-tags (cons (main-root-node) '(address article aside blockquote body canvas dd div dl dt fieldset figcaption figure footer form  h1 h2 h3 h4 h5 h6 header hgroup hr li main nav noscript ol output p pre section table tfoot ul video)))

(define-settable newline "\n")
(define-settable linebreak-separator (newline))
(define-settable paragraph-separator "\n\n")

(define-settable paths-excluded-from-dashboard (map string->path (list "poldash.css" "compiled")))

(define-settable project-server-port 8080)

(define+provide current-server-port (make-parameter (project-server-port)))
(define+provide current-server-listen-ip (make-parameter #f))

(define+provide current-render-source (make-parameter #f))

;; used to distinguish one-shot rendering (e.g., using `raco pollen setup` or `render`
;; from an interactive session with the project server (using `raco pollen start`)
;; in one-shot mode, certain features needed for dynamic recompilation are disabled for speed.
(define+provide current-session-interactive? (make-parameter #false))

(define-settable dashboard-css "poldash.css")

(define-runtime-path server-extras-dir "private/server-extras")
(define+provide current-server-extras-path (make-parameter server-extras-dir))

(define-settable publish-directory "publish")

(define-settable compile-cache-active #t)
(define-settable render-cache-active #t)
(define-settable compile-cache-max-size (* 10 1024 1024)) ; = 10 megabytes

(define-settable unpublished-path? (λ (path) #f)) ; deprecated in favor of `omitted-path?`
(define-settable omitted-path? (λ (path) #f))

(define-settable extra-published-path? (λ (path) #f)) ; deprecated in favor of `extra-path?`
(define-settable extra-path? (λ (path) #f))

(define-settable trim-whitespace? #t)

(define-settable poly-targets '(html)) ; current target applied to multi-output source files
(define+provide current-poly-target (make-parameter (car (poly-targets))))

(define-settable index-pages '("index.html"))

(define-settable allow-unbound-ids? #true)
