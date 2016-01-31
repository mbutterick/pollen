#lang racket/base
(require racket/runtime-path racket/file pollen/private/version)

(define-runtime-path info-file "../../info.rkt")

(module+ main
(define str (file->string info-file))
(define newstr
  (regexp-replace #rx"\\(define version .*?\\)" str (format "(define version ~v)" pollen:version)))
(display-to-file newstr info-file #:exists 'replace))