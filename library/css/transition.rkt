#lang racket/base
(require "core.rkt")

(provide (all-defined-out))

(define (make-css-transition property duration #:timing-function [timing-function #f] #:delay [delay #f])
  (define transition-prefixes '("-moz-" "-webkit-" ""))
  (join-css-strings  (append
                      (make-css-strings transition-prefixes "transition-property" property)
                      (make-css-strings transition-prefixes "transition-duration" duration)
                      (if timing-function
                          (make-css-strings transition-prefixes "transition-timing-function" timing-function)
                          empty)
                      (if delay
                          (make-css-strings transition-prefixes "transition-delay" delay)
                          empty))))
