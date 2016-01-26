#lang racket/base
(require (for-syntax racket/base racket/syntax) "private/version.rkt")
(require racket/runtime-path)

(provide (prefix-out world: (combine-out (all-defined-out) (all-from-out "private/version.rkt"))))

(define current-project-root (make-parameter (current-directory)))

(define directory-require "pollen.rkt")
(define env-name "POLLEN")

(define (get-path-to-override [file-or-dir (current-directory)])
  (define file-with-config-submodule directory-require)
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
(define-for-syntax world-submodule-name 'world)
(define-syntax (define-settable stx)
  (syntax-case stx ()
    [(_ name default-value)
     (with-syntax ([base-name (format-id stx "~a" #'name)]
                   [current-name (format-id stx "current-~a" #'name)]
                   [world-submodule (format-id stx "~a" world-submodule-name)]
                   [fail-thunk-name (format-id stx "fail-thunk-~a" #'name)] )
       #'(begin
           (define base-name default-value)
           (define fail-thunk-name (λ _ base-name))
           ;; can take a dir argument that sets start point for (get-path-to-override) search.
           (define current-name (λ get-path-args
                                  (with-handlers ([exn:fail? fail-thunk-name])
                                    (dynamic-require `(submod ,(apply get-path-to-override get-path-args) world-submodule) 'base-name fail-thunk-name))))))]))

(define-settable preproc-source-ext 'pp)
(define-settable markup-source-ext 'pm)
(define-settable markdown-source-ext 'pmd)
(define-settable null-source-ext 'p)
(define-settable pagetree-source-ext 'ptree)
(define-settable template-source-ext 'pt)
(define-settable scribble-source-ext 'scrbl)

;; these are deliberately not settable because they're just internal signalers, no effect on external interface
(define mode-auto 'auto)
(define mode-preproc 'pre)
(define mode-markup 'markup)
(define mode-markdown 'markdown)
(define mode-pagetree 'ptree)
(define mode-template 'template)

(define-settable cache-filename "pollen.cache")
(define-settable cache-dir-name "pollen-cache")
(define cache-names (list (current-cache-filename) (current-cache-dir-name)))

(define-settable decodable-extensions (list (current-markup-source-ext) (current-pagetree-source-ext)))

(define-settable default-pagetree (format "index.~a" (current-pagetree-source-ext)))
(define-settable pagetree-root-node 'pagetree-root)
(define-settable main-root-node 'root)

(define-settable command-char #\◊)
(define-settable template-command-char #\∂)

(define-settable default-template-prefix "template")
(define-settable fallback-template-prefix "fallback")
(define-settable template-meta-key "template")

(define-settable main-export 'doc) ; don't forget to change fallback template too
(define-settable meta-export 'metas)
(define-settable meta-tag-name 'meta)
(define-settable define-meta-name 'define-meta)

;; tags from https://developer.mozilla.org/en-US/docs/Web/HTML/Block-level_elements
(define-settable block-tags (cons (current-main-root-node) '(address article aside blockquote body canvas dd div dl fieldset figcaption figure footer form  h1 h2 h3 h4 h5 h6 header hgroup hr li main nav noscript ol output p pre section table tfoot ul video)))

(define-settable newline "\n")
(define-settable linebreak-separator (current-newline))
(define-settable paragraph-separator "\n\n")

(define-settable paths-excluded-from-dashboard (map string->path (list "poldash.css" "compiled")))


(define-settable default-port 8080)

(define current-server-port (make-parameter (current-default-port)))

(define-settable dashboard-css "poldash.css")

(define-runtime-path server-extras-dir "private/server-extras")
(define current-server-extras-path (make-parameter server-extras-dir))

(define-settable publish-directory-name "publish")

(define-settable extension-escape-char #\_)

(define-settable compile-cache-active #t)
(define-settable render-cache-active #t)
(define-settable compile-cache-max-size (* 10 1024 1024)) ; = 10 megabytes

(define-settable unpublished-path? (λ(path) #f))

(define-settable here-path-key 'here-path)

(define-settable poly-source-ext 'poly) ; extension that signals source can be used for multiple output targets
(define-settable poly-targets '(html)) ; current target applied to multi-output source files
(define current-poly-target (make-parameter (car (current-poly-targets))))