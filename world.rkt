#lang racket/base

(provide (all-defined-out))

(define POLLEN_PREPROC_EXT 'pp)
(define POLLEN_SOURCE_EXT 'p)
(define TEMPLATE_FILE_PREFIX "-")
(define POLLEN_EXPRESSION_DELIMITER #\◊)
(define TEMPLATE_FIELD_DELIMITER POLLEN_EXPRESSION_DELIMITER)

(define DEFAULT_TEMPLATE_PREFIX "-main")
(define FALLBACK_TEMPLATE_NAME "-temp-fallback-template.html")
(define TEMPLATE_META_KEY "template")

(define POLLEN_TREE_EXT 'ptree)
(define DEFAULT_POLLEN_TREE "main.ptree")
(define POLLEN_TREE_PARENT_NAME 'parent)
(define POLLEN_TREE_ROOT_NAME 'ptree-root)

(define MAIN_POLLEN_EXPORT 'main)
;(define META_POLLEN_TAG 'metas)
;(define META_POLLEN_EXPORT 'metas)

(define EXTRAS_DIR (string->path "require"))

(define MISSING_FILE_BOILERPLATE "#lang planet mb/pollen\n\n")

(define LINE_BREAK "\n")
(define PARAGRAPH_BREAK "\n\n")

(define OUTPUT_SUBDIR 'public)

(define RACKET_PATH "/usr/bin/racket")

(define POLLEN_ROOT 'main)

; get the starting directory, which is the parent of 'run-file
(define POLLEN_PROJECT_DIR
  (let-values ([(dir ignored also-ignored)
                (split-path (find-system-path 'run-file))])
    (if (equal? dir 'relative)
        (string->path ".")
        dir)))


