#lang racket/base
(require "readability.rkt")
(require racket/contract racket/match)
(require (only-in racket/path filename-extension))
(require (only-in racket/format ~a))
(require (only-in xml xexpr?))
(provide (all-defined-out))

;; setup for test cases
(module+ test
  (require rackunit)
  (define foo-path-strings '("foo" "foo.txt" "foo.bar" "foo.bar.txt"))
  (define-values (foo-path foo.txt-path foo.bar-path foo.bar.txt-path) (apply values (map string->path foo-path-strings)))
  ;; test the sample paths before using them for other tests
  (define foo-paths (list foo-path foo.txt-path foo.bar-path foo.bar.txt-path))
  (for-each check-equal? (map path->string foo-paths) foo-path-strings))


;; does path have a certain extension
(define/contract (has-ext? path ext)
  (path? symbol? . -> . boolean?)
  (define ext-of-path (filename-extension path))
  (and ext-of-path (equal? (bytes->string/utf-8 ext-of-path) (as-string ext))))

(module+ test
  (check-equal? (has-ext? foo-path 'txt) #f)
  (check-equal? (has-ext? foo.txt-path 'txt) #t)
  (check-equal? (has-ext? foo.bar.txt-path 'txt) #t))


;; take one extension off path
(define/contract (remove-ext path)
  (path? . -> . path?)
  (path-replace-suffix path ""))

(module+ test  
  (check-equal? (remove-ext foo-path) foo-path)
  (check-equal? (remove-ext foo.txt-path) foo-path)
  (check-equal? (remove-ext foo.bar.txt-path) foo.bar-path))


;; take all extensions off path
(define/contract (remove-all-ext path)
  (path? . -> . path?)
  (define path-with-removed-ext (remove-ext path))
  (if (equal? path path-with-removed-ext)
      path
      (remove-all-ext path-with-removed-ext)))

(module+ test  
  (check-equal? (remove-all-ext foo-path) foo-path)
  (check-equal? (remove-all-ext foo.txt-path) foo-path)
  (check-equal? (remove-all-ext foo.bar.txt-path) foo-path))


;; is it an xexpr attributes?
(define/contract (xexpr-attrs? x)
  (any/c . -> . boolean?)
  (define (attr-pair? x)
    ; list with two elements: first element is a symbol, second is a string
    (and (list? x) (= (length x) 2) (symbol? (car x)) (string? (cadr x))))   
  ; a list where elements are attr pairs
  (and (list? x) (andmap attr-pair? x)))

;; is it xexpr content?
(define/contract (xexpr-content? x)
  (any/c . -> . boolean?)
  (and (list? x) (andmap xexpr? x)))

;; is it a named x-expression?
;; todo: rewrite this recurively so errors can be pinpointed (for debugging)
(define/contract (named-xexpr? x)
  (any/c . -> . boolean?)
  (and (xexpr? x) ; meets basic xexpr contract
       (match x
         [(list (? symbol? name) rest ...) ; is a list starting with a symbol
          (or (xexpr-content? rest) ; the rest is content or ...
              (and (xexpr-attrs? (car rest)) (xexpr-content? (cdr rest))))] ; attributes followed by content
         [else #f])))

(module+ test  
  (check-equal? (named-xexpr? "foo") #f)
  (check-equal? (named-xexpr? '(p "foo" "bar")) #t)
  (check-equal? (named-xexpr? '(p ((key "value")) "foo" "bar")) #t)
  (check-equal? (named-xexpr? '(p "foo" "bar" ((key "value")))) #f)
  (check-equal? (named-xexpr? '("p" "foo" "bar")) #f)
  (check-equal? (named-xexpr? '(p 123)) #t)) ; why is this so?
