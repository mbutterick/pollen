#lang racket/base
(require racket/contract racket/list racket/match)
(require (planet mb/pollen/tools) (planet mb/pollen/decode))

(provide (all-defined-out))

(module+ test (require rackunit))

;; todo: contracts & unit tests
(define/contract (meta-proc meta)
  (meta-xexpr? . -> . named-xexpr?)
  `(meta ((name ,(second meta))(content ,(third meta)))))

(module+ test
  (check-equal? (meta-proc '(meta "key" "value")) '(meta ((name "key")(content "value")))))


;; how a paragraph break is denoted: two or more newlines
(define/contract (paragraph-break? x)
  (any/c . -> . boolean?)
  (and (string? x) (->boolean (regexp-match #px"^\n{2,}$" x))))
  
(module+ test
  (check-false (paragraph-break? "foo"))
  (check-false (paragraph-break? "\n"))
  (check-true (paragraph-break? "\n\n"))
  (check-true (paragraph-break? "\n\n\n")))


;; convert single newline to br tag
;; only if neither adjacent tag is a block
;; otherwise delete
;; todo: contracts & unit tests
(define (convert-linebreaks x)  ; x is list
  (filter-not empty?
              (for/list ([i (len x)])
                (cond
                  [(equal? (get x i) "\n") ; todo: don't hardcode this
                   (if (andmap (λ(i) (not (block-xexpr? i))) (list (get x (sub1 i)) (get x (add1 i))))
                       '(br)
                       '())]
                  [else (get x i)]))))

;; todo: contracts & unit tests
(define (prep-paragraph-flow x)
  (convert-linebreaks (merge-newlines (trim x whitespace?))))

(module+ test
  (check-equal? (prep-paragraph-flow '("\n" "foo" "\n" "\n" "bar" "\n" "ino" "\n"))
                '("foo" "\n\n" "bar" (br) "ino")))

;; todo: contracts & unit tests
(define (wrap-paragraph x) ; x is a list containing paragraph pieces
  ; if paragraph is just one block-level xexpr
  (if (and (= (length x) 1) (block-xexpr? (car x))) 
      (car x) ; leave it
      `(p ,@x))) ; otherwise wrap in p tag

;; todo: contracts & unit tests
(define (xexpr-content-proc content)
  (let ([content (prep-paragraph-flow content)]) 
    (if (ormap paragraph-break? content) ; need this condition to prevent infinite recursion
        (map wrap-paragraph (splitf-at* content paragraph-break?)) ; split into ¶¶
        content)))

(define (root . items)
  (named-xexpr? . -> . named-xexpr?)
  (decode (cons 'root items)
          ;          #:exclude-xexpr-names 'em
          ;          #:xexpr-name-proc [xexpr-name-proc (λ(x)x)]
          ;          #:xexpr-attr-proc [xexpr-attr-proc (λ(x)x)]
          #:xexpr-content-proc xexpr-content-proc
          ;          #:block-xexpr-proc [block-xexpr-proc (λ(x)x)]
          ;          #:inline-xexpr-proc [inline-xexpr-proc (λ(x)x)]
          ;         #:string-proc string-proc
          #:meta-proc meta-proc
          ))


(define foo "bar")