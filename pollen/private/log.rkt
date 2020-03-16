#lang racket/base
(require racket/format
         racket/string
         "external/logging.rkt")

(provide (all-defined-out) (all-from-out "external/logging.rkt"))

;; creates `pollen-logger` and associated functions:
;; log-pollen-fatal, log-pollen-error, log-pollen-warning, 
;; log-pollen-info, and log-pollen-debug
(define-logger pollen) 

(define (message . items)
  (log-pollen-info (string-join (map ~a items) " ")))

(define (message-debug . items)
  (log-pollen-debug (string-join (map ~a items) " ")))
