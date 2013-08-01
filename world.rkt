#lang racket

; in the dev branch

(define POLLEN_PREPROC_EXT 'pp)
(define POLLEN_SOURCE_EXT 'p)
(define POLLEN_MAP_EXT 'pmap)
(define TEMPLATE_FILE_PREFIX #\-)
(define POLLEN_EXPRESSION_DELIMITER #\◊)
(define TEMPLATE_FIELD_DELIMITER POLLEN_EXPRESSION_DELIMITER)

(define DEFAULT_TEMPLATE "-main.html")
(define TEMPLATE_META_KEY 'template)

(define DEFAULT_MAP "main.pmap")

(define MAIN_POLLEN_EXPORT 'body)
;(define META_POLLEN_TAG 'metas)
;(define META_POLLEN_EXPORT 'metas)

(define EXTRAS_DIR (string->path "requires"))

(define MISSING_FILE_BOILERPLATE "#lang planet mb/pollen\n\n")

(define LINE_BREAK "\n")
(define PARAGRAPH_BREAK "\n\n")

(define OUTPUT_SUBDIR 'public)

(define RACKET_PATH "/Applications/Racket/bin/racket")

(define POLLEN_ROOT 'main)

; todo: this doesn't work as hoped
;(define-syntax POLLEN_ROOT_TAG
;  (λ(stx) (datum->syntax stx 'main)))

; get the starting directory, which is the parent of 'run-file
(define START_DIR
  (let-values ([(dir ignored also-ignored)
                (split-path (find-system-path 'run-file))])
    (if (equal? dir 'relative)
        (string->path ".")
        dir)))

(provide (all-defined-out))