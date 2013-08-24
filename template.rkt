#lang racket/base
(require racket/list racket/contract racket/string xml xml/path racket/bool)
(require "readability.rkt" "debug.rkt" "predicates.rkt" "tools.rkt")

;; setup for test cases
(module+ test (require rackunit racket/path))

(provide (all-defined-out))

;; todo: better fallback template

(define fallback-template-data "FALLBACK! ◊(put-as-html main)")

;; todo: docstrings for this subsection

(define/contract (puttable-item? x)
  (any/c . -> . boolean?)
  (or (tagged-xexpr? x) (has-pollen-source? x)))

(define/contract (query-key? x)
  (any/c . -> . boolean?)
  (or (string? x) (symbol? x)))

(define/contract (put x)
  (puttable-item? . -> . tagged-xexpr?)
  (cond
    ;; Using put has no effect on tagged-xexprs. It's here to make the idiom smooth.
    [(tagged-xexpr? x) x] 
    [(has-pollen-source? x) (dynamic-require (make-pollen-source-path x) 'main)]))

(module+ test
  (check-equal? (put '(foo "bar")) '(foo "bar"))
  (check-equal? (put "tests/template/put.p") 
                '(root "\n" "\n" (em "One") " paragraph" "\n" "\n" "Another " (em "paragraph") "\n" "\n")))



(define/contract (find-in x query)
  (puttable-item? query-key? . -> . (or/c xexpr-elements? false?))
  (or (find-in-metas x query) (find-in-main x query)))

(module+ test 
  (parameterize ([current-directory "tests/template"])
    (check-false (find-in "put" "nonexistent-key"))
    (check-equal? (find-in "put" "foo") (list "bar"))
    (check-equal? (find-in "put" "em") (list "One" "paragraph"))))

(define/contract (find-in-metas x key)
  (puttable-item? query-key? . -> . (or/c xexpr-elements? false?))
  (and (has-pollen-source? x)
       (let ([metas (dynamic-require (make-pollen-source-path x) 'metas)]
             [key (->string key)])
         (and (key . in? . metas ) (->list (get metas key))))))

(module+ test
  (parameterize ([current-directory "tests/template"])
    (check-equal? (find-in-metas "put" "foo") (list "bar"))
    (let* ([metas (dynamic-require (make-pollen-source-path 'put) 'metas)]
           [here (find-in-metas 'put 'here)]
           [here-relative (list (->string (find-relative-path (current-directory) (car here))))])     
      (check-equal? here-relative (list "put.p")))))


(define/contract (find-in-main x query) 
  (puttable-item? (or/c query-key? (listof query-key?)) 
                  . -> . (or/c xexpr-elements? false?))
  (let* ([x (put x)]
         ;; make sure query is a list of symbols (required by se-path*/list)
         [query (map ->symbol (->list query))]
         [results (se-path*/list query x)])
    ;; if results exist, send back xexpr as output
    (and (not (empty? results)) results)))

(module+ test
  (parameterize ([current-directory "tests/template"])
    (check-false (find-in-main "put" "nonexistent-key"))
    (check-equal? (find-in-main "put" "em") (list "One" "paragraph"))))


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
