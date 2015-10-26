#lang racket/base
(require txexpr sugar/define racket/string racket/match)


(define first car)
(define second cadr)
(define+provide make-default-tag-function
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


(define/contract+provide (split-attributes parts)
  (list? . -> . (values txexpr-attrs? txexpr-elements?))
  (define dummy-tag (gensym))
  (define dummy-txexpr (apply (make-default-tag-function dummy-tag) parts))
  (define-values (tag attrs elements) (txexpr->values dummy-txexpr))
  (values attrs elements))


(module+ test
  (require rackunit)
  (define outerdiv (make-default-tag-function 'div #:class "outer" #:style "outer"))
  (check-equal? (outerdiv "foo") '(div ((class "outer") (style "outer")) "foo"))
  (check-equal? (outerdiv) '(div ((class "outer") (style "outer"))))
  (check-equal? (outerdiv #:class "inner") '(div ((class "outer") (style "outer") (class "inner"))))
  (check-equal? (outerdiv #:class "inner" "foo") '(div ((class "outer") (style "outer") (class "inner")) "foo"))
  ;; `make-keyword-procedure` sorts keyword arguments alphabetically, so 'field' ends up before 'id'
  (check-equal? (outerdiv #:id "shazbot" #:field "greens" "foo") '(div ((class "outer") (style "outer") (field "greens") (id "shazbot")) "foo"))
  (check-equal? (outerdiv 'id: "shazbot" "foo") '(div ((class "outer") (style "outer") (id "shazbot")) "foo"))
  (check-equal? (outerdiv '((id "shazbot")) "foo") '(div ((class "outer") (style "outer") (id "shazbot")) "foo"))
  (check-equal? (outerdiv 'id: "shazbot" 'class: "inner" "foo") '(div ((class "outer") (style "outer") (id "shazbot") (class "inner")) "foo"))
  ;; (outerdiv 'id: "shazbot" '((class "inner")) "foo") won't work because colon attrs supplant conventional attrs (docs concur)
  (check-equal? (outerdiv 'id: "shazbot" #:class "inner" "foo") '(div ((class "outer") (style "outer") (class "inner") (id "shazbot")) "foo")))