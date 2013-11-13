#lang racket/base
(require "core.rkt")

(provide (all-defined-out))


; editability can't be handled as pure css because firefox requires extra content-editable attribute.
; does it still? todo: further research, maybe this can be css only.
(define (editable . stuff)
  (define editable-string (make-css-editable))
  `(div ((style ,editable-string)(contenteditable "true")) ,@stuff))

(define (make-css-editable)
  (join-css-strings (list "user-modify: read-write"
                          "-moz-user-modify: read-write"
                          "-webkit-user-modify: read-write-plaintext-only"
                          "outline-style: none")))


(define (make-media-query starting-size ending-size max-width interval)
  (string-join (cons (format "@media all {html {font-size: ~apx;}}" starting-size)
                     (for/list ([size (in-range starting-size (sub1 ending-size) -1)])
                       (format "@media all and (max-width:~apx){html {font-size: ~apx;}}" 
                               (- max-width (* interval (- starting-size size))) size))) "\n"))


(module+ main
  (display (make-media-query 15 11 980 60)))