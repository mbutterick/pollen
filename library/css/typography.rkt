#lang racket/base
(require "core.rkt")

(provide (all-defined-out))

(define (make-css-ot-features feature-tags [feature-values 1])
  ; if single value provided, upconvert to list
  (when (not (list? feature-tags))
    (set! feature-tags (list feature-tags)))
  
  ; same here: convert single value into list
  (when (not (list? feature-values))
    (let ([single-value feature-values])
      (set! feature-values (make-list (len feature-tags) single-value))))
  
  ; use single quotes in the formatter because css string might be used in an inline tag
  ; with form style="[string]" so double quotes are irritating
  (define feature-tag-string (string-join (map (λ(tag value) (format "'~a' ~a" tag value)) feature-tags feature-values) ", "))
  
  ; I hate accommodating old browsers but I'll make an exception because OT support is 
  ; critical to most MB projects
  ; if this comes before new-style -moz- declaration, it will work for all.
  (define feature-tag-string-old-firefox (string-join (map (λ(tag value) (format "'~a=~a'" tag value)) feature-tags feature-values) ", "))
  
  (define feature-tag-property "font-feature-settings")
  
  (join-css-strings (append
                     (make-css-strings '("-moz-") feature-tag-property feature-tag-string-old-firefox)
                     (make-css-strings css-property-prefixes feature-tag-property feature-tag-string))))


(define (make-css-hyphens [value "auto"])
  (join-css-strings (make-css-strings css-property-prefixes "hyphens" value)))

(define (make-css-small-caps)
  (join-css-strings (list "text-transform: lowercase" (make-css-ot-features "c2sc"))))

(define (make-css-caps)
  (join-css-strings (list "text-transform: uppercase" (make-css-ot-features "case"))))

(define (make-css-kerning)
  (join-css-strings (list "text-rendering: optimizeLegibility" (make-css-ot-features "kern"))))


(define (make-css-ligatures)
  (join-css-strings (list "text-rendering: optimizeLegibility" (make-css-ot-features "liga"))))