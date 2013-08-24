#lang racket/base
(require racket/list racket/contract racket/string xml xml/path racket/bool)
(require "readability.rkt" "debug.rkt" "predicates.rkt" "tools.rkt")

;; setup for test cases
(module+ test (require rackunit racket/path))

(provide (all-defined-out))

;; todo: better fallback template

(define fallback-template-data "FALLBACK! ◊(put-as-html main)")

;; todo: tests & contracts for this subsection

(define/contract (puttable-item? x)
  (any/c . -> . boolean?)
  (or (tagged-xexpr? x) (has-pollen-source? x)))

(define/contract (put x)
  (puttable-item? . -> . tagged-xexpr?)
  (cond
    ;; Using put has no effect on tagged-xexprs. It's here to make the idiom smooth.
    [(tagged-xexpr? x) x] 
    [(has-pollen-source? x) (dynamic-require (make-pollen-source-path x) 'main)]))

(module+ test
  (check-equal? (put '(foo "bar")) '(foo "bar"))
  (check-equal? (put "tests/template/put.p") 
                '(root "\n" "\n" "One paragraph" "\n" "\n" "Another paragraph" "\n" "\n")))



(define/contract (from x query)
  (puttable-item? (or/c string? symbol?) . -> . (or/c list? false?))
  (or 
   (and (has-pollen-source? x) (from-metas x query))
   (from-main x query)))


(define/contract (from-metas x key)
  (has-pollen-source? (or/c string? symbol?) . -> . (or/c list? false?))
  (let ([metas (dynamic-require (make-pollen-source-path x) 'metas)]
        [key (->string key)])
    ;; todo: why am I returning value as xexpr?
    (and (key . in? . metas ) `(value ,(get metas key)))))

(module+ test
  (parameterize ([current-directory "tests/template"])
    (let ([metas (dynamic-require (make-pollen-source-path 'put) 'metas)])
    (check-equal? (from-metas "put" "foo") '(value "bar"))
    (check-equal? (from-metas 'put 'here) `(value ,(find-relative-path (current-directory) (->path (get metas "here"))))))))

(define (from-main x query) ; this used to be plain from
  ; check results first
  (let* ([x (put x)]
         [results (se-path*/list (list query) x)])
    ; if results exist, send back xexpr as output
    (if (not (empty? results))
        `(,query ,@results) ; todo: why use query as tag?
        #f)))


(define (merge x)
  (cond
    [(tagged-xexpr? x)
     ; return content of xexpr.
     ; pollen language rules will splice these into the main flow.
     (if (empty? x)
         ""
         (let-values([(name attr content) (break-tagged-xexpr x)])
           content))]
    [(string? x) (list x)]))


#|(define (merge-strings x)
  (when (empty? x) (error "merge-strings got empty x"))
  ;todo: filter metas?
  ; leaning toward no. Simplest behavior.
  ; function is not intended to be used with whole pollen body anyhow.
  (let ([x (merge x)])
    (string-join (filter string? (flatten x)) " ")))|#

(define (merge-strings x)
  (string-join (filter string? (flatten x)) " "))


(define (make-html x)
  (if (tagged-xexpr? x)
      (xexpr->string x)
      (let ([x (->list x)])
        (when (andmap xexpr? x)
          (string-join (map xexpr->string x) "")))))

; generate *-as-html versions of functions
(define-values (put-as-html merge-as-html merge-strings-as-html)
  (apply values (map (λ(proc) (λ(x) (make-html (proc x)))) (list put merge merge-strings))))
