#lang racket/base
(require sugar
         txexpr/base
         racket/list
         racket/string
         pollen/setup
         xml
         html
         racket/match
         "html.rkt"
         pollen/unstable/typography
         net/url
         racket/port)

(module+ test
  (require rackunit))

(define (attrs->pollen-attrs attrs)
  (string-join (for*/list ([attr (in-list attrs)]
                           [k (in-value (car attr))]
                           [v (in-value (cadr attr))])
                          (format "#:~a ~v" k v)) " "))

(define/contract+provide (xexpr->pollen x #:white-p? [white-p? #f])
  ((xexpr?) (#:white-p? boolean?) . ->* . string?)
  (let loop ([x x])
    (cond
      [(and white-p? (txexpr? x) (eq? (car x) 'p) (empty? (get-attrs x))
            (string-append* (list* (setup:paragraph-separator) (map ->string (map loop (get-elements x))))))]
      [(and (txexpr? x)
            (or (not (empty? (get-attrs x)))
                (not (empty? (get-elements x)))))
       (string-append* (map ->string `(,(setup:command-char)
                                       ,(get-tag x) 
                                       ,@(if (pair? (get-attrs x)) `("[" ,(attrs->pollen-attrs (get-attrs x)) "]") null) 
                                       ,@(if (pair? (get-elements x)) `("{" ,@(map loop (get-elements x)) "}" ) null))))]
      [(txexpr? x) ; no attrs or tag, so needs parens
       (format "~a(~a)" (setup:command-char) (get-tag x))]
      [(symbol? x) (loop (entity->integer x))]
      [(number? x) (format "~a" (integer->char x))]
      [else x])))

(module+ test
  (check-equal? (xexpr->pollen '(p "You are puppy")) "◊p{You are puppy}")
  (check-equal? (xexpr->pollen '(div (p "You are puppy") (p "You are kitty"))) "◊div{◊p{You are puppy}◊p{You are kitty}}")
  (check-equal? (xexpr->pollen #:white-p? #t '(div (p "You are puppy") (p "You are kitty"))) "◊div{

You are puppy

You are kitty}")
  (check-equal? (xexpr->pollen '(p ((class "foo")) "You are puppy")) "◊p[#:class \"foo\"]{You are puppy}")
  (check-equal? (xexpr->pollen '(p)) "◊(p)")
  (check-equal? (xexpr->pollen '(p ((class "foo")) "You are " (em "so") " puppy")) "◊p[#:class \"foo\"]{You are ◊em{so} puppy}"))

(define (conjoin . fs)
  (λ (x) (andmap (λ (f) (f x)) fs)))

(define/contract+provide (html->xexpr html-string)
  (string? . -> . xexpr?)
  (use-html-spec #f)
  (define xexpr-results
    ; loop result will be a list with whitespace elements, so strip those out
    (filter-not (conjoin string? whitespace?) 
                (let loop ([elem (read-html-as-xml (open-input-string html-string))])
                  (match elem
                    [(struct pcdata (start stop string)) string]
                    [(struct entity (start stop entity)) entity]
                    [(struct attribute (start stop key value)) (list key value)]
                    [(struct element (start stop name attributes content)) `(,name ,@(if (empty? attributes) empty (list (map loop attributes))) ,@(map loop content))]
                    [(list elements ...) (map loop elements)]
                    [else (format "unknown item: ~a" elem)]))))  
  (if (pair? xexpr-results) (car xexpr-results) ""))

(module+ test
  (define (hx-identity xexpr)
    (equal? (html->xexpr (xexpr->html xexpr)) xexpr))
  (check-true (hx-identity '(p "You are puppy")))
  (check-true (hx-identity '(p ((class "foo")) "You are puppy")))
  (check-true (hx-identity '(p)))
  (check-true (hx-identity '(p ((class "foo")) "You are " (em "so") " puppy")))
  (check-equal? (html->xexpr "\n") ""))


(define/contract+provide (html->pollen html-string #:white-p? [white-p? #f])
  ((string?) (#:white-p? boolean?) . ->* . string?)
  (xexpr->pollen #:white-p? white-p? (html->xexpr html-string))) 


(define/contract+provide (url->pollen url-or-string #:white-p? [white-p? #f])
  (((or/c string? url?)) (#:white-p? boolean?) . ->* . string?)
  (define url (if (string? url-or-string) (string->url url-or-string) url-or-string))
  (define url-result (port->string (get-pure-port url)))
  (html->pollen url-result #:white-p? white-p?))
