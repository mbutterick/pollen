#lang racket/base
(require racket/list "debug.rkt" "predicates.rkt")
(provide (all-defined-out)
         (all-from-out  racket/list "debug.rkt" "predicates.rkt"))


(require xml racket/string)

(define (html->xexpr . xs)
  (string->xexpr (string-join xs "")))
