#lang pollen/mode racket/base
(require (for-syntax
          racket/base
          syntax/parse)
         txexpr/base
         racket/string
         racket/match)
(provide default-tag-function make-default-tag-function define-tag-function)

(define (parse-leading-attrs xs)
  (match xs
    [(cons (? txexpr-attrs? leading-attrs) tail) (values leading-attrs tail)]
    [else (values null xs)]))

(define (colon-attr-name? x)
  (match x
    [(? symbol?)
     (=> resume)
     (match (symbol->string x)
       [(regexp #rx".*?(?=:$)" (cons res _)) (string->symbol res)]
       [_ (resume)])]
    [_ #false]))

(define (parse-colon-attrs xs)
  (let parse-next ([xs xs][colon-attrs empty])
    (match xs
      [(list* (? colon-attr-name? name) (? string? val) xs)
       (parse-next xs (cons (list (colon-attr-name? name) val) colon-attrs))]
      [_ (values colon-attrs xs)])))

(define (parse-kw-attrs kw-symbols-in kw-args)
  (define kw-symbols (map (λ (kw) (string->symbol (string-trim (keyword->string kw) "#:"))) kw-symbols-in))
  (map list kw-symbols kw-args))

(define (make-one-tag-function outer-kws outer-kw-args id)
  (make-keyword-procedure
   (λ (inner-kws inner-kw-args . xs)
     ;; Three possible sources of attrs:
     ;; 1) normal attrs, in a list at the front of the args
     (let*-values ([(leading-attrs xs) (parse-leading-attrs xs)]
                   ;; 2) colon attrs, using special 'key: "value" syntax, also at the front of the args
                   [(colon-attrs xs) (parse-colon-attrs xs)]
                   ;; 3) keyword attrs
                   [(kw-attrs) (parse-kw-attrs (append outer-kws inner-kws) (append outer-kw-args inner-kw-args))])
       ;; construct the xexpr result "manually" (i.e., not with `make-txexpr` because it may not be a legit txexpr for now
       ;; (but it may become one through further processing, so no need to be finicky)
       ;; however, don't show empty attrs.
       (cons id (match (append kw-attrs colon-attrs leading-attrs)
                  [(== empty) xs]
                  [attrs (cons attrs xs)]))))))

(define default-tag-function
  (make-keyword-procedure
   (λ (outer-kws outer-kw-args . ids)
     (define tag-proc (apply compose1 (for/list ([id (in-list ids)])
                                                (make-one-tag-function outer-kws outer-kw-args id))))
     (define tag-proc-name (string->symbol (format "pollen-tag:~a" (string-join (map symbol->string ids) "+"))))
     (procedure-rename tag-proc tag-proc-name))))

(define make-default-tag-function default-tag-function) ; bw compat


(module+ test
  (require txexpr/check)
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
    [(THIS (ID:id ARG:id ...) EXPR:expr ...)
     #'(THIS ID (λ (ARG ...) EXPR ...))]
    [(_ ID:id (λ (ATTRS:id ELEMS:id ARG:id ...) EXPR:expr ...))
     #:fail-when (> (length (syntax->list #'(ARG ...))) 0) "tag function must have exactly 2 positional arguments"
     #:fail-when (check-duplicate-identifier (list #'ATTRS #'ELEMS)) "duplicate variable name"
     #:fail-when (null? (syntax->list #'(EXPR ...))) "body of definition cannot be empty"
     ;; the srcloc of the `lambda` expression determines the srcloc of errors raised within its body
     #`(define ID
         (make-keyword-procedure
          #,(syntax/loc #'ID (lambda (kws kwargs . args)
                               (let ([elems (match args
                                              [(list* _ elems) elems]
                                              [_ #false])])
                                 (when elems
                                   (unless (and (list? elems) (andmap txexpr-element? elems))
                                     (raise-argument-error 'ID (format "elements need to be passed to tag function as individual trailing arguments (or, if you want to pass them as a single list, use `(apply ~a ···)` here instead of `(~a ···)`)" 'ID 'ID) (car elems)))))
                               (define tx-proc (keyword-apply default-tag-function kws kwargs (list 'ID)))
                               (define tx (apply tx-proc args))
                               (define-values (_ ATTRS ELEMS) (txexpr->values tx))
                               EXPR ...))))]))


(module+ test
  (define foo2 (default-tag-function 'foo))
  (define-tag-function (foo attrs elems)
    `(foo ,(reverse attrs) ,@elems))
  (check-txexprs-equal? ◊(foo) ◊(foo2))
  (check-txexprs-equal? ◊foo[#:zim "zam"]{hello}  ◊foo2[#:zim "zam"]{hello})
  (check-txexprs-equal? ◊foo[#:ding "dong" '((zim "zam"))]{hello}  ◊foo2[#:ding "dong" '((zim "zam"))]{hello})
  (check-txexprs-equal? ◊foo['zim: "zam" #:ding "dong" ]{hello} ◊foo2['zim: "zam" #:ding "dong" ]{hello})
  
  (define-tag-function foolam (λ (attrs elems)
                                `(foo ,(reverse attrs) ,@elems)))
  (check-txexprs-equal? ◊foolam[#:zim "zam"]{hello}  ◊foo2[#:zim "zam"]{hello})
  (check-txexprs-equal? ◊foolam[#:ding "dong" '((zim "zam"))]{hello}  ◊foo2[#:ding "dong" '((zim "zam"))]{hello})
  (check-txexprs-equal? ◊foolam['zim: "zam" #:ding "dong" ]{hello} ◊foo2['zim: "zam" #:ding "dong" ]{hello}))