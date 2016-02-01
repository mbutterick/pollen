#lang pollen/mode racket/base
(require (for-syntax racket/base syntax/parse))
(require txexpr racket/string racket/match)
(provide default-tag-function make-default-tag-function define-tag-function)

(define first car)
(define second cadr)
(define default-tag-function
  (make-keyword-procedure
   (λ (outer-kws outer-kw-args . ids)
     (define (make-one-tag id)
       (make-keyword-procedure
        (λ (inner-kws inner-kw-args . attrs+xs)
          
          ;; Three possible sources of attrs:
          ;; 1) normal attrs, in a list at the front of the args
          ;; 2) colon args, using special 'key: "value" syntax, also at the front of the args
          ;; 3) keyword args.
          
          (define-values (leading-attrs xs) (if (and (pair? attrs+xs) (txexpr-attrs? (car attrs+xs)))
                                                (values (car attrs+xs) (cdr attrs+xs))
                                                (values null attrs+xs)))                                               
          
          (define-values (kws kw-args) (values (append outer-kws inner-kws) (append outer-kw-args inner-kw-args)))
          
          (match-define (list colon-attrs ... body) (let parse-one-colon-attr ([xs xs])
                                                      (define (colon-attr-name? x) (let ([result (regexp-match #rx".*?(?=:$)" (symbol->string x))])
                                                                                     (and result (string->symbol (car result))))) ; return name or #f
                                                      (define maybe-attr-name (and (>= (length xs) 2) 
                                                                                   (symbol? (first xs)) 
                                                                                   (string? (second xs)) ; accept strings only as attr value
                                                                                   (colon-attr-name? (first xs))))
                                                      (if maybe-attr-name
                                                          (let ([attr-name maybe-attr-name][attr-value (second xs)])
                                                            (cons (list attr-name attr-value) (parse-one-colon-attr (cddr xs))))
                                                          (list xs))))
          (define kw-symbols (map (λ(kw) (string->symbol (string-trim (keyword->string kw) "#:"))) kws))
          (define attrs (append (map list kw-symbols kw-args) colon-attrs leading-attrs))
          
          ;; construct the xexpr result "manually" (i.e., not with `make-txexpr` because it may not be a legit txexpr for now
          ;; (but it may become one through further processing, so no need to be finicky)
          ;; however, don't show empty attrs.
          (list* id (if (null? attrs)
                        body
                        (list* attrs body))))))
     
     (let ([tag-proc (apply compose1 (map make-one-tag ids))]
           [tag-proc-name (string->symbol (format "pollen-tag:~a" (string-join (map symbol->string ids) "+")))])
       (procedure-rename tag-proc tag-proc-name)))))

(define make-default-tag-function default-tag-function) ; bw compat

(module+ test
  (require rackunit)
  (define outerdiv (default-tag-function 'div #:class "outer" #:style "outer"))
  (check-txexprs-equal? (outerdiv "foo") '(div ((class "outer") (style "outer")) "foo"))
  (check-txexprs-equal? (outerdiv) '(div ((class "outer") (style "outer"))))
  (check-txexprs-equal? (outerdiv #:class "inner") '(div ((class "outer") (style "outer") (class "inner"))))
  (check-txexprs-equal? (outerdiv #:class "inner" "foo") '(div ((class "outer") (style "outer") (class "inner")) "foo"))
  (check-txexprs-equal? (outerdiv #:field "greens" #:id "shazbot" "foo") '(div ((class "outer") (style "outer") (field "greens") (id "shazbot")) "foo"))
  (check-txexprs-equal? (outerdiv 'id: "shazbot" "foo") '(div ((class "outer") (style "outer") (id "shazbot")) "foo"))
  (check-txexprs-equal? (outerdiv '((id "shazbot")) "foo") '(div ((class "outer") (style "outer") (id "shazbot")) "foo"))
  (check-txexprs-equal? (outerdiv 'id: "shazbot" 'class: "inner" "foo") '(div ((class "outer") (style "outer") (id "shazbot") (class "inner")) "foo"))
  ;; (outerdiv 'id: "shazbot" '((class "inner")) "foo") won't work because colon attrs supplant conventional attrs (docs concur)
  (check-txexprs-equal? (outerdiv 'id: "shazbot" #:class "inner" "foo") '(div ((class "outer") (style "outer") (class "inner") (id "shazbot")) "foo")))


(define-syntax (define-tag-function stx)
  (syntax-parse stx
    #:literals (λ)
    [(_ (ID:id ARG:id ...) EXPR:expr ...)
     #'(define-tag-function ID (λ (ARG ...) EXPR ...))]
    [(_ ID:id (λ (ATTRS:id ELEMS:id ARG:id ...) EXPR:expr ...))
     #:fail-when (> (length (syntax->list #'(ARG ...))) 0) "tag function must have exactly 2 positional arguments"
     #:fail-when (check-duplicate-identifier (list #'ATTRS #'ELEMS)) "duplicate variable name"
     #:fail-when (null? (syntax->list #'(EXPR ...))) "body of definition cannot be empty"
     #'(define ID
         (make-keyword-procedure
          (λ (kws kwargs . args)
            (define tx-proc (keyword-apply default-tag-function kws kwargs (list 'ID)))
            (define tx (apply tx-proc args))
            (define-values (_ ATTRS ELEMS) (txexpr->values tx))
            EXPR ...)))]))


(module+ test
  (require rackunit)
  (define foo2 (default-tag-function 'foo))
  
  (define-tag-function (foo attrs elems)
    `(foo ,(reverse attrs) ,@elems))
  (check-txexprs-equal? ◊foo[#:zim "zam"]{hello}  ◊foo2[#:zim "zam"]{hello})
  (check-txexprs-equal? ◊foo[#:ding "dong" '((zim "zam"))]{hello}  ◊foo2[#:ding "dong" '((zim "zam"))]{hello})
  (check-txexprs-equal? ◊foo['zim: "zam" #:ding "dong" ]{hello} ◊foo2['zim: "zam" #:ding "dong" ]{hello})
  
  (define-tag-function foolam (λ (attrs elems)
                                `(foo ,(reverse attrs) ,@elems)))
  (check-txexprs-equal? ◊foolam[#:zim "zam"]{hello}  ◊foo2[#:zim "zam"]{hello})
  (check-txexprs-equal? ◊foolam[#:ding "dong" '((zim "zam"))]{hello}  ◊foo2[#:ding "dong" '((zim "zam"))]{hello})
  (check-txexprs-equal? ◊foolam['zim: "zam" #:ding "dong" ]{hello} ◊foo2['zim: "zam" #:ding "dong" ]{hello}))