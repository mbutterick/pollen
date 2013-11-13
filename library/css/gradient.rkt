#lang racket/base
(require "core.rkt")

(provide (all-defined-out))

(define (make-css-background-gradient colors [stops #f] 
                                      #:radial [radial #f] 
                                      #:horizontal [horizontal #f]
                                      #:direction [direction #f])
  ; this doesn't handle old-style webkit syntax. todo: add it? I think I don't care
  
  ; check inputs for failure
  (when (or (not (list? colors)) (< (len colors) 2))
    (error "Not enough colors to make gradient in" colors))
  (when (and stops (< (len stops) (len colors)))
    (error "Not enough stops for given number of colors in" stops))
  
  (when (not stops) ; distribute colors evenly between 0 and 100
    ; new-stops is range of steps incremented properly and rounded to int, then append 100 to end
    (let ([new-stops `(,@(map ->int (range 0 100 (/ 100 (sub1 (len colors))))) 100)])
      ; convert to list of percentages
      (set! stops (map (λ(x) (format "~a%" x)) new-stops))))
  
  ; color / percentage pairs separated by commas
  (define color-stop-string (string-join (map (λ(color stop) (format "~a ~a" color stop)) colors stops) ", "))
  
  ; set up gradient options
  (define gradient-type (if radial "radial" "linear"))
  (define gradient-direction (or direction (if horizontal "left" "top")))
  
  ; can't use standard make-css-strings in this case because the prefixes appear in the value,
  ; not in the property (which is always "background")
  (define gradient-strings (map (λ(prefix) (format "background: ~a~a-gradient(~a, ~a)" prefix gradient-type gradient-direction color-stop-string)) css-property-prefixes))
  
  ; just fill with the last color if gradient not available
  (define fallback-string (format "background: ~a" (last colors)))
  
  ; put fallback string at front of list
  (join-css-strings (cons fallback-string gradient-strings)))

(module+ main
  (display (make-css-background-gradient (list "hsl(216, 78%, 95%)" "hsl(0, 0%, 99%)") 
                                         (list "0%" "100%")
                                         #:direction "to top right")))