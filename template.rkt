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



(define/contract (find px query)
  (puttable-item? query-key? . -> . (or/c xexpr-elements? false?))
  (or (find-in-metas px query) (find-in-main px query)))

(module+ test 
  (parameterize ([current-directory "tests/template"])
    (check-false (find "put" "nonexistent-key"))
    (check-equal? (find "put" "foo") (list "bar"))
    (check-equal? (find "put" "em") (list "One" "paragraph"))))

(define/contract (find-in-metas px key)
  (puttable-item? query-key? . -> . (or/c xexpr-elements? false?))
  (and (has-pollen-source? px)
       (let ([metas (dynamic-require (make-pollen-source-path px) 'metas)]
             [key (->string key)])
         (and (key . in? . metas ) (->list (get metas key))))))

(module+ test
  (parameterize ([current-directory "tests/template"])
    (check-equal? (find-in-metas "put" "foo") (list "bar"))
    (let* ([metas (dynamic-require (make-pollen-source-path 'put) 'metas)]
           [here (find-in-metas 'put 'here)]
           [here-relative (list (->string (find-relative-path (current-directory) (car here))))])     
      (check-equal? here-relative (list "put.p")))))


(define/contract (find-in-main px query) 
  (puttable-item? (or/c query-key? (listof query-key?)) 
                  . -> . (or/c xexpr-elements? false?))
  (let* ([px (put px)]
         ;; make sure query is a list of symbols (required by se-path*/list)
         [query (map ->symbol (->list query))]
         [results (se-path*/list query px)])
    ;; if results exist, send back xexpr as output
    (and (not (empty? results)) results)))

(module+ test
  (parameterize ([current-directory "tests/template"])
    (check-false (find-in-main "put" "nonexistent-key"))
    (check-equal? (find-in-main "put" "em") (list "One" "paragraph"))))


;; turns input into xexpr-elements so they can be spliced into template
;; (as opposed to dropped in as a full tagged-xexpr)
;; by returning a list, pollen rules will automatically merge into main flow
;; todo: explain why
;; todo: do I need this?
(define/contract (splice x)
  ((or/c tagged-xexpr? xexpr-elements? string?) . -> . xexpr-elements?)
  (cond
    [(tagged-xexpr? x) (tagged-xexpr-elements x)]
    [(xexpr-elements? x) x]
    [(string? x) (->list x)]))

(module+ test
  (check-equal? (splice '(p "foo" "bar")) (list "foo" "bar"))
  (check-equal? (splice (list "foo" "bar")) (list "foo" "bar"))
  (check-equal? (splice "foo") (list "foo")))


(define/contract (make-html x)
  ((or/c tagged-xexpr? xexpr-elements? xexpr-element?) . -> . string?)
  (cond
    [(tagged-xexpr? x) (xexpr->string x)]
    [else (let ([x (->list x)])
            (string-join (map xexpr->string x) ""))]))

; generate *-as-html versions of functions
(define-values (put-as-html splice-as-html)
  (apply values (map (λ(proc) (λ(x) (make-html (proc x)))) (list put splice))))
