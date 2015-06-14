#lang racket/base
(require racket/runtime-path)

(provide (prefix-out world: (all-defined-out)))

(define pollen-version "0.001")

(define preproc-source-ext 'pp)
(define markup-source-ext 'pm)
(define markdown-source-ext 'pmd)
(define null-source-ext 'p)
(define pagetree-source-ext 'ptree)
(define template-source-ext 'pt)
(define scribble-source-ext 'scrbl)

(define mode-auto 'auto)
(define mode-preproc 'pre)
(define mode-markup 'markup)
(define mode-markdown 'markdown)
(define mode-pagetree 'ptree)
(define mode-template 'template)

(define cache-filename "pollen.cache")

(define decodable-extensions (list markup-source-ext pagetree-source-ext))

(define default-pagetree "index.ptree")
(define pagetree-root-node 'pagetree-root)

(define command-marker #\◊)
(define template-command-marker #\∂)

(define default-template-prefix "template")
(define fallback-template-prefix "fallback")
(define template-meta-key "template")

(define main-pollen-export 'doc) ; don't forget to change fallback template too
(define meta-pollen-export 'metas)

(define directory-require "directory-require.rkt")

(define newline "\n")
(define linebreak-separator newline)
(define paragraph-separator "\n\n")

(define paths-excluded-from-dashboard
  (map string->path (list "poldash.css" "compiled")))


(define current-project-root (make-parameter (current-directory)))

(define default-port 8080)
(define current-server-port (make-parameter default-port))

(define dashboard-css "poldash.css")

(define-runtime-path server-extras-dir "server-extras")
(define current-server-extras-path (make-parameter server-extras-dir))

(define check-directory-requires-in-render? (make-parameter #t))

(define publish-directory-name "publish")
