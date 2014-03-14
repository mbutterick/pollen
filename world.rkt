#lang racket/base

(provide (prefix-out world: (all-defined-out)))

(define pollen-version "0.001")

(define preproc-source-ext 'pp)
(define markup-source-ext 'pm)
(define markdown-source-ext 'pmd)
(define null-source-ext 'p)
(define pagemap-source-ext 'pmap)
(define template-source-ext 'pt)
(define scribble-source-ext 'scrbl)


(define reader-mode-auto 'auto)
(define reader-mode-preproc 'pre)
(define reader-mode-markup 'markup)
(define reader-mode-markdown 'markdown)
(define reader-mode-pagemap 'pmap)

(define decodable-extensions (list markup-source-ext pagemap-source-ext))

(define default-pagemap "index.pmap")
(define pagemap-root-node 'pagemap-root)

(define template-source-prefix "-")
(define expression-delimiter #\◊)
(define template-field-delimiter expression-delimiter)

(define default-template-prefix "main")
(define fallback-template "fallback.html.pt")
(define template-meta-key "template")

(define main-pollen-export 'doc) ; don't forget to change fallback template too
(define meta-pollen-export 'metas)

(define pollen-require "pollen-require.rkt")

(define missing-file-boilerplace "#lang pollen\n\n")

(define newline "\n")
(define linebreak-separator newline)
(define paragraph-separator "\n\n")

(define output-subdir 'public)

(define racket-path "/usr/bin/racket")

(define command-file "polcom")

(define reserved-paths
  (map string->path (list command-file "poldash.css" "compiled")))


(define current-project-root (make-parameter (current-directory)))

(define server-port 8088)

(define dashboard-name "index.pmap")
(define dashboard-css "poldash.css")

(define current-module-root (make-parameter #f))
(define current-server-extras-path (make-parameter #f))

(define check-project-requires-in-render? (make-parameter #t))
