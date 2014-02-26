#lang racket/base

;; todo: how to make project- or user-specific prefs

(provide (all-defined-out))

(define POLLEN_VERSION "0.001")

(define PREPROC_SOURCE_EXT 'p)
(define MARKUP_SOURCE_EXT 'pm)
(define NULL_SOURCE_EXT 'px)
(define PTREE_SOURCE_EXT 'ptree)
(define DECODABLE_EXTENSIONS (list MARKUP_SOURCE_EXT PTREE_SOURCE_EXT))

(define DEFAULT_PTREE "main.ptree")
(define PTREE_ROOT_NODE 'ptree-root)

(define TEMPLATE_SOURCE_PREFIX "-")
(define EXPRESSION_DELIMITER #\◊)
(define TEMPLATE_FIELD_DELIMITER EXPRESSION_DELIMITER)

(define DEFAULT_TEMPLATE_PREFIX "main")
(define TEMPLATE_EXT 'pt)
(define FALLBACK_TEMPLATE "fallback.html.pt")
(define TEMPLATE_META_KEY "template")

(define MAIN_POLLEN_EXPORT 'main)

(define EXTRAS_DIR (string->path "pollen-require"))

(define MISSING_FILE_BOILERPLATE "#lang pollen\n\n")

(define LINE_BREAK "\n")
(define PARAGRAPH_BREAK "\n\n")

(define OUTPUT_SUBDIR 'public)

;;(require racket/string racket/port racket/system)
;; todo: is path to racket already available as an environment variable?
;; e.g., (find-system-path 'xxx)? Because this next line is sort of slow
;;(define RACKET_PATH (string-trim (with-output-to-string (λ() (system "which racket")))))
(define RACKET_PATH "/usr/bin/racket") ;; todo: this won't always work

(define COMMAND_FILE "polcom")

(define RESERVED_PATHS
  (map string->path (list COMMAND_FILE (path->string EXTRAS_DIR) "poldash.css" "compiled")))


(define CURRENT_PROJECT_ROOT (make-parameter (current-directory)))

(define SERVER_PORT 8088)

(define DASHBOARD_NAME "index.ptree")
(define DASHBOARD_CSS "poldash.css")

(define current-module-root (make-parameter #f))
(define current-server-extras-path (make-parameter #f))


(define reader-mode-auto 'auto)
(define reader-mode-preproc 'pre)
(define reader-mode-markup 'markup)
(define reader-mode-ptree 'ptree)
