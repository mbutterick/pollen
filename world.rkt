#lang racket/base
(require (for-syntax racket/base racket/syntax))
(require racket/runtime-path)

(provide (prefix-out world: (all-defined-out)))

(define current-project-root (make-parameter (current-directory)))

(define directory-require "directory-require.rkt")
(define (get-path-to-override) 
  (build-path (current-project-root) directory-require))

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
           (define current-name (λ _ (with-handlers ([exn:fail? fail-thunk-name])
                                    (dynamic-require `(submod ,(get-path-to-override) config-submodule) 'base-name fail-thunk-name))))))]))

(define-settable pollen-version "0.001")

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

(define-settable newline "\n")
(define-settable linebreak-separator (current-newline))
(define-settable paragraph-separator "\n\n")

(define-settable paths-excluded-from-dashboard (map string->path (list "poldash.css" "compiled")))


(define-settable default-port 8080)

(define current-server-port (make-parameter (current-default-port)))

(define-settable dashboard-css "poldash.css")

(define-runtime-path server-extras-dir "server-extras")
(define current-server-extras-path (make-parameter server-extras-dir))

(define check-directory-requires-in-render? (make-parameter #t))

(define-settable publish-directory-name "publish")