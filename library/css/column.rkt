#lang racket/base
(require "core.rkt")

(provide (all-defined-out))

(define (make-css-columns #:count count #:gap [gap #f])
  ; shorthand for css column declaration
  (join-css-strings  (append
                      (make-css-strings css-property-prefixes "column-count" count)
                      (if gap
                          (make-css-strings css-property-prefixes "column-gap" gap)
                          empty))))

(define (make-css-avoid-column-break-inside)
  ; this gets applied to list items to keep them from breaking across columns
  ; however it doesn't work in Firefox due to bug; workaround is stupid
  (join-css-strings (append
                     (make-css-strings css-property-prefixes "column-break-inside" "avoid")
                     (make-css-strings css-property-prefixes "break-inside" "avoid-column"))))



