#lang racket

(provide (all-defined-out))

(define POLLEN_VERSION "0.001")

(define POLLEN_PREPROC_EXT 'p)
(define POLLEN_DECODER_EXT 'pd)
(define POLLEN_TREE_EXT 'ptree)

(define DEFAULT_POLLEN_TREE "main.ptree")
(define POLLEN_TREE_PARENT_NAME 'parent)
(define POLLEN_TREE_ROOT_NAME 'ptree-root)

(define TEMPLATE_FILE_PREFIX "-")
(define POLLEN_EXPRESSION_DELIMITER #\◊)
(define TEMPLATE_FIELD_DELIMITER POLLEN_EXPRESSION_DELIMITER)

(define DEFAULT_TEMPLATE_PREFIX "-main")
(define FALLBACK_TEMPLATE_NAME "-temp-fallback-template.html")
(define TEMPLATE_META_KEY "template")


(define MAIN_POLLEN_EXPORT 'main)
;(define META_POLLEN_TAG 'metas)
;(define META_POLLEN_EXPORT 'metas)

(define EXTRAS_DIR (string->path "pollen-require"))

(define MISSING_FILE_BOILERPLATE "#lang pollen\n\n")

(define LINE_BREAK "\n")
(define PARAGRAPH_BREAK "\n\n")

(define OUTPUT_SUBDIR 'public)

(require racket/string racket/port racket/system)
;; todo: is path to racket already available as an environment variable?
;; e.g., (find-system-path 'xxx)? Because this next line is sort of slow
;;(define RACKET_PATH (string-trim (with-output-to-string (λ() (system "which racket")))))
(define RACKET_PATH "/usr/bin/racket") ;; todo: this won't always work

(define POLLEN_ROOT 'main)
(define POLLEN_COMMAND_FILE "polcom")

(require "readability.rkt")
(define RESERVED_PATHS
  (map ->path (list POLLEN_COMMAND_FILE EXTRAS_DIR "poldash.css" "compiled")))


(define PROJECT_ROOT (current-directory))
(define (reset-project-root) (set! PROJECT_ROOT (current-directory)))
;; use current-contract-region to calculate containing directory of module
(define MODULE_ROOT (apply build-path (drop-right (explode-path (current-contract-region)) 1)))
(define SERVER_EXTRAS_DIR (build-path MODULE_ROOT "pollen-server-extras"))

(define SERVER_PORT 8088)

(define DASHBOARD_NAME "index.ptree")
(define DASHBOARD_CSS "poldash.css")