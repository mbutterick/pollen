#lang racket/base
(require sugar txexpr racket/list racket/string pollen/world xml html racket/file racket/match pollen/html net/url racket/port)

(define (attrs->pollen attrs)
  (string-join (flatten (map (λ(pair) (list (format "'~a:" (car pair)) (format "\"~a\"" (cadr pair)))) attrs)) " "))


(define/contract+provide (xexpr->pollen x #:p-breaks [p-breaks #f])
  ((xexpr?) (#:p-breaks boolean?) . ->* . string?)
  
  (let loop ([x x])
    (cond
      [(and p-breaks (txexpr? x) (equal? (car x) 'p) (apply string-append `("\n" ,@(map ->string (map loop (get-elements x))) "\n")))]
      [(txexpr? x) (apply string-append 
                          (map ->string  `(,(world:current-command-char) ,(get-tag x) 
                                                                 ,@(if (not (null? (get-attrs x))) `("[" ,(attrs->pollen (get-attrs x)) "]") null) 
                                                                 ,@(if (not (null? (get-elements x))) `("{" ,@(map loop (get-elements x)) "}" ) null))))]
      [(symbol? x) (loop (entity->integer x))]
      [(number? x) (format "~a" (integer->char x))]
      [else x])))


(define/contract+provide (html->xexpr html-string)
  (string? . -> . xexpr?)
  (use-html-spec #f)
  (define xexpr-results 
    (let loop ([elem (read-html-as-xml (open-input-string html-string))])
      (match elem
        [(struct pcdata (start stop string)) string]
        [(struct entity (start stop entity)) entity]
        [(struct attribute (start stop key value)) (list key value)]
        [(struct element (start stop name attributes content)) `(,name ,(map loop attributes) ,@(map loop content))]
        [(list elements ...) (map loop elements)]
        [else (format "unknown item: ~a" elem)])))

  ; xexpr-results will be a list with whitespace elements, so strip those out
  (car (filter-not (λ(x) (and (string? x) (regexp-match #px"\\s+" x))) xexpr-results)))


(define/contract+provide (html->pollen html-string #:p-breaks [p-breaks #f])
  ((string?) (#:p-breaks boolean?) . ->* . string?)
  (xexpr->pollen #:p-breaks p-breaks (html->xexpr html-string))) 


(define/contract+provide (url->pollen url-or-string #:p-breaks [p-breaks #f])
  (((or/c string? url?)) (#:p-breaks boolean?) . ->* . string?)
  (define url (if (string? url-or-string) (string->url url-or-string) url-or-string))
  (define url-result (port->string (get-pure-port url)))
  (html->pollen url-result  #:p-breaks p-breaks))

(module+ main
  ; (xexpr->pollen '(p "You are puppy"))
  ; (xexpr->pollen '(p ((class "foo")) "You are puppy"))
  ; (xexpr->pollen '(p ((class "foo")) "You are" "\n\n" "puppy"))
  ; (xexpr->pollen '(p ((class "foo")) "You are " (em "so") " puppy"))
  ;  (display (html->pollen #:p-breaks #t (file->string "index.html"))))
)