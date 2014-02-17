#lang racket/base
(require racket/contract racket/string xml xml/path racket/bool)
(require "tools.rkt" "ptree.rkt" sugar/scribble sugar/coerce sugar tagged-xexpr)

;; setup for test cases
(module+ test (require rackunit racket/path))

(provide (all-defined-out))
(provide (all-from-out sugar/scribble sugar/coerce))

;; todo: better fallback template

(define fallback-template-data "FALLBACK! ◊(put-as-html main)")

;; todo: docstrings for this subsection

(define/contract (puttable-item? x)
  (any/c . -> . boolean?)
  (or (tagged-xexpr? x) 
      (has-decoder-source? x) 
      (and (pnode? x) (pnode->url x) (has-decoder-source? (pnode->url x)))))

(module+ test
  (check-false (puttable-item? #t))
  (check-false (puttable-item? #f)))

(define/contract (query-key? x)
  (any/c . -> . boolean?)
  (or (string? x) (symbol? x)))

(define/contract (put x)
  (puttable-item? . -> . tagged-xexpr?)
  (cond
    ;; Using put has no effect on tagged-xexprs. It's here to make the idiom smooth.
    [(tagged-xexpr? x) x] 
    [(has-decoder-source? x) (dynamic-require (->decoder-source-path x) 'main)]
    [(has-decoder-source? (pnode->url x)) (dynamic-require (->decoder-source-path (pnode->url x)) 'main)]))

#|(module+ test
  (check-equal? (put '(foo "bar")) '(foo "bar"))
  (check-equal? (put "tests/template/put.pd") 
                '(root "\n" "\n" (em "One") " paragraph" "\n" "\n" "Another " (em "paragraph") "\n" "\n")))
|#


(define/contract (find query px)
  (query-key? (or/c false? puttable-item?) . -> . (or/c false? xexpr-element?))
  (define result (and px (or (find-in-metas px query) (find-in-main px query))))
  (and result (car result))) ;; return false or first element

#|
(module+ test 
  (parameterize ([current-directory "tests/template"])
    (check-false (find "nonexistent-key" "put"))
    (check-equal? (find "foo" "put") "bar")
    (check-equal? (find "em" "put") "One"))
  (check-equal? (find "foo" #f) #f))
|#

(define/contract (find-in-metas px key)
  (puttable-item? query-key? . -> . (or/c false? xexpr-elements?))
  (and (has-decoder-source? px)
       (let ([metas (dynamic-require (->decoder-source-path px) 'metas)]
             [key (->string key)])
         (and (key . in? . metas ) (->list (get metas key))))))

#|(module+ test
  (parameterize ([current-directory "tests/template"])
    (check-equal? (find-in-metas "put" "foo") (list "bar"))
    (let* ([metas (dynamic-require (->decoder-source-path 'put) 'metas)]
           [here (find-in-metas 'put 'here)])     
      (check-equal? here (list "tests/template/put")))))
|#

(define/contract (find-in-main px query) 
  (puttable-item? (or/c query-key? (listof query-key?)) 
                  . -> . (or/c  false? xexpr-elements?))
  (let* ([px (put px)]
         ;; make sure query is a list of symbols (required by se-path*/list)
         [query (map ->symbol (->list query))]
         [results (se-path*/list query px)])
    ;; if results exist, send back xexpr as output
    (and (not (empty? results)) results)))

#|
(module+ test
  (parameterize ([current-directory "tests/template"])
    (check-false (find-in-main "put" "nonexistent-key"))
    (check-equal? (find-in-main "put" "em") (list "One" "paragraph"))))
|#

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

(define ->html put-as-html)




