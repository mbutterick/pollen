#lang racket/base
(require (for-syntax racket/base racket/syntax))
(require racket/runtime-path)

(define-syntax-rule (define+provide id expr ...)
  (begin
    (provide id)
    (define id expr ...)))

(define+provide current-project-root (make-parameter (current-directory)))

(define+provide default-directory-require "pollen.rkt")
(define+provide default-env-name "POLLEN")

(define (get-path-to-override [file-or-dir (current-directory)])
  (define file-with-config-submodule default-directory-require)
  (define (dirname path)
    (let-values ([(dir name dir?) (split-path path)])
      dir))
  (define starting-dir (if (directory-exists? file-or-dir) file-or-dir (dirname file-or-dir)))
  (let loop ([dir starting-dir][path file-with-config-submodule])
    (and dir ; dir is #f when it hits the top of the filesystem
         (let ([completed-path (path->complete-path path starting-dir)])
           (if (file-exists? completed-path)
               (simplify-path completed-path)
               (loop (dirname dir) (build-path 'up path)))))))


;; parameters should not be made settable.
(define-for-syntax world-submodule-name 'setup)
(define-syntax (define-settable stx)
  (syntax-case stx ()
    [(_ name default-value)
     (with-syntax ([default-name (format-id stx "default-~a" #'name)]
                   [name-thunked (format-id stx "~a" #'name)]
                   [world-submodule (format-id stx "~a" world-submodule-name)]
                   [name-fail-thunked (format-id stx "fail-thunk-~a" #'name)] )
       #'(begin
           (provide (prefix-out setup: name-thunked) default-name)
           (define default-name default-value)
           (define name-fail-thunked (λ _ default-name))
           ;; can take a dir argument that sets start point for (get-path-to-override) search.
           (define name-thunked (λ get-path-args
                                  (with-handlers ([exn:fail? name-fail-thunked])
                                    (dynamic-require `(submod ,(apply get-path-to-override get-path-args) world-submodule) 'name name-fail-thunked))))))]))

(define-settable preproc-source-ext 'pp)
(define-settable markup-source-ext 'pm)
(define-settable markdown-source-ext 'pmd)
(define-settable null-source-ext 'p)
(define-settable pagetree-source-ext 'ptree)
(define-settable template-source-ext 'pt)
(define-settable scribble-source-ext 'scrbl)

;; these are deliberately not settable because they're just internal signalers, no effect on external interface
(define+provide default-mode-auto 'auto)
(define+provide default-mode-preproc 'pre)
(define+provide default-mode-markup 'markup)
(define+provide default-mode-markdown 'markdown)
(define+provide default-mode-pagetree 'ptree)
(define+provide default-mode-template 'template)

(define-settable cache-filename "pollen.cache")
(define-settable cache-dir-name "pollen-cache")
(define+provide default-cache-names (list (cache-filename) (cache-dir-name)))

(define-settable decodable-extensions (list (markup-source-ext) (pagetree-source-ext)))

(define-settable main-pagetree (format "index.~a" (pagetree-source-ext)))
(define-settable pagetree-root-node 'pagetree-root)
(define-settable main-root-node 'root)

(define-settable command-char #\◊)
(define-settable template-command-char #\∂)

(define-settable template-prefix "template")
(define-settable fallback-template-prefix "fallback")
(define-settable template-meta-key "template")

(define-settable main-export 'doc) ; don't forget to change fallback template too
(define-settable meta-export 'metas)
(define-settable meta-tag-name 'meta)
(define-settable define-meta-name 'define-meta)

;; tags from https://developer.mozilla.org/en-US/docs/Web/HTML/Block-level_elements
(define-settable block-tags (cons (main-root-node) '(address article aside blockquote body canvas dd div dl fieldset figcaption figure footer form  h1 h2 h3 h4 h5 h6 header hgroup hr li main nav noscript ol output p pre section table tfoot ul video)))

(define-settable newline "\n")
(define-settable linebreak-separator (newline))
(define-settable paragraph-separator "\n\n")

(define-settable paths-excluded-from-dashboard (map string->path (list "poldash.css" "compiled")))


(define-settable project-server-port 8080)

(define+provide current-server-port (make-parameter (project-server-port)))

(define-settable dashboard-css "poldash.css")

(define-runtime-path server-extras-dir "private/server-extras")
(define+provide current-server-extras-path (make-parameter server-extras-dir))

(define-settable publish-directory-name "publish")

(define-settable extension-escape-char #\_)

(define-settable compile-cache-active #t)
(define-settable render-cache-active #t)
(define-settable compile-cache-max-size (* 10 1024 1024)) ; = 10 megabytes

(define-settable unpublished-path? (λ(path) #f))

(define-settable here-path-key 'here-path)

(define-settable splicing-tag '@)

(define-settable poly-source-ext 'poly) ; extension that signals source can be used for multiple output targets
(define-settable poly-targets '(html)) ; current target applied to multi-output source files
(define+provide current-poly-target (make-parameter (car (poly-targets))))