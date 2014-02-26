#lang racket/base

(provide (prefix-out world: (all-defined-out)))

(define pollen-version "0.001")

(define preproc-source-ext 'p)
(define markup-source-ext 'pm)
(define null-source-ext 'px)
(define ptree-source-ext 'ptree)
(define template-source-ext 'pt)

(define reader-mode-auto 'auto)
(define reader-mode-preproc 'pre)
(define reader-mode-markup 'markup)
(define reader-mode-ptree 'ptree)

(define decodable-extensions (list markup-source-ext ptree-source-ext))

(define default-ptree "main.ptree")
(define ptree-root-node 'ptree-root)

(define template-source-prefix "-")
(define expression-delimiter #\◊)
(define template-field-delimiter expression-delimiter)

(define default-template-prefix "main")
(define fallback-template "fallback.html.pt")
(define template-meta-key "template")

(define main-pollen-export 'main)

(define extras-dir (string->path "pollen-require"))

(define missing-file-boilerplace "#lang pollen\n\n")

(define line-break "\n")
(define paragraph-break "\n\n")

(define output-subdir 'public)

(define racket-path "/usr/bin/racket")

(define command-file "polcom")

(define reserved-paths
  (map string->path (list command-file (path->string extras-dir) "poldash.css" "compiled")))


(define current-project-root (make-parameter (current-directory)))

(define server-port 8088)

(define dashboard-name "index.ptree")
(define dashboard-css "poldash.css")

(define current-module-root (make-parameter #f))
(define current-server-extras-path (make-parameter #f))


