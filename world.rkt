#lang racket/base
(require (for-syntax racket/base racket/syntax))
(require racket/runtime-path)

(provide (prefix-out world: (all-defined-out)))

(define current-project-root (make-parameter (current-directory)))

(define directory-require "pollen.rkt")

(define (get-path-to-override [starting-dir (current-directory)])
  ;; for now, let `path->complete-path` flag any argument errors (test here is redundant)
  #;(when (or (not (path? starting-dir)) (not (directory-exists? starting-dir)))
    (error 'get-path-to-override (format "~a is not a directory" starting-dir)))
  (define file-with-config-submodule directory-require)
  (define (dirname path)
    (let-values ([(dir name dir?) (split-path path)])
      dir))  
  (let loop ([dir starting-dir][path file-with-config-submodule])
    (and dir ; dir is #f when it hits the top of the filesystem
         (let ([completed-path (path->complete-path path starting-dir)])
           (if (file-exists? completed-path)
               (simplify-path completed-path)
               (loop (dirname dir) (build-path 'up path)))))))


;; parameters should not be made settable.
(define-for-syntax config-submodule-name 'config)
(define-syntax (define-settable stx)
  (syntax-case stx ()
    [(_ name default-value)
     (with-syntax ([base-name (format-id stx "~a" #'name)]
                   [current-name (format-id stx "current-~a" #'name)]
                   [config-submodule (format-id stx "~a" config-submodule-name)]
                   [fail-thunk-name (format-id stx "fail-thunk-~a" #'name)] )
       #'(begin
           (define base-name default-value)
           (define fail-thunk-name (λ _ base-name))
           ;; can take a dir argument that sets start point for (get-path-to-override) search.
           (define current-name (λ get-path-args
                                  (with-handlers ([exn:fail? fail-thunk-name])
                                    (dynamic-require `(submod ,(apply get-path-to-override get-path-args) config-submodule) 'base-name fail-thunk-name))))))]))

(define-settable pollen-version "0.1508")

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

(define-settable command-char #\◊)
(define-settable template-command-char #\∂)

(define-settable default-template-prefix "template")
(define-settable fallback-template-prefix "fallback")
(define-settable template-meta-key "template")

(define-settable main-export 'doc) ; don't forget to change fallback template too
(define-settable meta-export 'metas)
(define-settable meta-tag-name 'meta)
(define-settable define-meta-name 'define-meta)

(define-settable newline "\n")
(define-settable linebreak-separator (current-newline))
(define-settable paragraph-separator "\n\n")

(define-settable paths-excluded-from-dashboard (map string->path (list "poldash.css" "compiled")))


(define-settable default-port 8080)

(define current-server-port (make-parameter (current-default-port)))

(define-settable dashboard-css "poldash.css")

(define-runtime-path server-extras-dir "server-extras")
(define current-server-extras-path (make-parameter server-extras-dir))

(define-settable publish-directory-name "publish")

(define-settable extension-escape-char #\_)

(define-settable compile-cache-active #t)
(define-settable render-cache-active #t)
(define-settable compile-cache-max-size (* 10 1024 1024)) ; = 10 megabytes

(define-settable unpublished-path? (λ(path) #f))

(define-settable here-path-key 'here-path)