#lang racket/base

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

(define decodable-extensions (list markup-source-ext pagetree-source-ext))

(define default-pagetree "index.ptree")
(define pagetree-root-node 'pagetree-root)

(define command-marker #\◊)
(define template-field-delimiter command-marker)

(define default-template-prefix "main")
(define fallback-template "fallback.html.pt")
(define template-meta-key "template")

(define main-pollen-export 'doc) ; don't forget to change fallback template too
(define meta-pollen-export 'metas)

(define project-require "project-require.rkt")

(define newline "\n")
(define linebreak-separator newline)
(define paragraph-separator "\n\n")

(define paths-excluded-from-dashboard
  (map string->path (list "poldash.css" "compiled")))


(define current-project-root (make-parameter (current-directory)))

(define current-server-port (make-parameter 8088))

(define dashboard-css "poldash.css")

(define server-extras-dir "server-extras")
(define current-server-extras-path (make-parameter #f))

(define check-project-requires-in-render? (make-parameter #t))
