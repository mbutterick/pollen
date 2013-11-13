#lang racket/base
(require "core.rkt" racket/match)

(define/contract (css-unit? x)
  (any/c . -> . boolean?)
  (x . in? . '("%" "in" "cm" "mm" "em" "ex" "pt" "pc" "px" "rem")))

(struct cssq (num unit)   
  #:methods gen:custom-write
  [(define write-proc 
     (λ(x port mode) (display (format "~a~a" (cssq-num x) (cssq-unit x)) port)))])

(define/contract (cssqish? x)
  (any/c . -> . boolean?)
  (->boolean (or (cssq? x) (cssqish->cssq x))))

(define/contract (string->unit x)
  (string? . -> . css-unit?)
  (if (css-unit? x)
      x
      (error 'string->unit "'~a' not a valid css unit" x)))

(define (cssqish->cssq x)
  (cond
    [(cssq? x) x]
    [else (begin
            (define pieces (let* ([str (string-downcase x)]
                                  [str (string-replace str " " "")]
                                  [str (string-trim str "s" #:left? #f)])
                             (string-split str #px"(?<![A-Za-z])(?=[A-Za-z])")))
            (apply cssq 
                   (with-handlers ([exn:fail? (λ(e) (error 'string->cssq (exn-message e)))])
                     (list (string->number (first pieces)) 
                           (string->unit (second pieces))))))]))

(define/contract (css-math-op op left right)
  (procedure? cssqish? cssqish? . -> . cssq?)
  (let ([left (cssqish->cssq left)]
        [right (cssqish->cssq right)])
    (cssqish->cssq (format "~a~a" (apply op (list (cssq-num left) (cssq-num right)))(cssq-unit left)))))

(define-values (css+ css- css* css/)
  (apply values (map (λ(op) (λ(left right) (css-math-op op left right))) (list + - * /))))

(module+ main
  (css+ "10rem" "5rem"))