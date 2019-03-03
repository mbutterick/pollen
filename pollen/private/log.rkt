#lang racket/base
(require racket/format
         racket/string
         racket/logging)

(provide (all-defined-out) (all-from-out racket/logging))

;; creates `pollen-logger` and associated functions:
;; log-pollen-fatal, log-pollen-error, log-pollen-warning, 
;; log-pollen-info, and log-pollen-debug
(define-logger pollen) 

(define (message . items)
  (log-pollen-info (string-join (map ~a items) " ")))
