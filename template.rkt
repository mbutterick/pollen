#lang racket/base
(require (for-syntax racket/base))
(require racket/string xml xml/path sugar/define sugar/container sugar/coerce sugar/test)
(require "file.rkt" txexpr "world.rkt" "cache.rkt" "pagetree.rkt" "debug.rkt")

(provide (all-from-out sugar/coerce))


(define/contract+provide (metas->here metas)
  (hash? . -> . pagenode?)
  (path->pagenode (or (select-from-metas 'here-path metas) 'unknown)))


(define (pagenode->path pagenode)
  (build-path (world:current-project-root) (symbol->string pagenode)))


(define+provide/contract (select key value-source)
  (coerce/symbol? (or/c hash? txexpr? pagenode? pathish?) . -> . (or/c #f txexpr-element?))
  ;; of value-source is specfically 'metas or 'doc,
  ;; restrict the search to there.
  ;; otherwise look in metas, then in doc for key
  (define (do-doc-result)
    (define doc-result (select-from-doc key value-source))
    (and doc-result (car doc-result)))  
  (cond
    [(or (hash? value-source) (equal? value-source world:meta-pollen-export)) (select-from-metas key value-source)]
    [(equal? value-source world:main-pollen-export) (do-doc-result)]
    [else
     (define metas-result (and (not (txexpr? value-source)) (select-from-metas key value-source)))
     (or metas-result (do-doc-result))]))

(module-test-external
 (check-equal? (select 'key  '#hash((key . "value"))) "value")
 (check-false (select 'absent-key  '#hash((key . "value"))))
 (check-equal? (select 'key  '(root (key "value"))) "value")
 (check-false (select 'absent-key  '(root (key "value"))))
 (let ([metas '#hash((key . "value"))])
   (check-equal? (select 'key  metas) "value")
   (check-false (select 'absent-key  metas)))
 (let ([doc '(root (key "value"))])
   (check-equal? (select 'key  doc) "value")
   (check-false (select 'absent-key  doc))))


(define+provide/contract (select* key value-source)
  (coerce/symbol? (or/c hash? txexpr? pagenode? pathish?) . -> . (or/c #f txexpr-elements?))
  (define metas-result (and (not (txexpr? value-source)) (select-from-metas key value-source)))
  (define doc-result (select-from-doc key value-source))
  (define result (append (or (and metas-result (list metas-result)) null) (or doc-result null)))
  (and (not (null? result)) result))


(define+provide/contract (select-from-metas key metas-source)
  (coerce/symbol? (or/c hash? pagenode? pathish?) . -> . (or/c #f txexpr-element?))
  (define metas (cond
                  [(hash? metas-source) metas-source]
                  [else (get-metas metas-source)]))
  (and (hash-has-key? metas key) (hash-ref metas key)))

(module-test-external
 (let ([metas '#hash((key . "value"))])
   (check-equal? (select-from-metas 'key  metas) "value")
   (check-false (select-from-metas 'absent-key  metas))))


(define+provide/contract (select-from-doc key doc-source)
  (coerce/symbol? (or/c txexpr? pagenode? pathish?) . -> . (or/c #f txexpr-elements?))
  (define doc (cond
                [(txexpr? doc-source) doc-source]
                [else (get-doc doc-source)]))
  (define result (se-path*/list (list key) doc))
  (and (not (null? result)) result))

(module-test-external
 (check-equal? (select-from-doc 'key  '(root (key "value"))) '("value"))
 (check-false (select-from-doc 'absent-key  '(root (key "value"))))
 (let ([doc '(root (key "value"))])
  (check-equal? (select-from-doc 'key  doc) '("value"))
  (check-false (select-from-doc 'absent-key  doc))))


(define (get-metas pagenode-or-path)
  ;  ((or/c pagenode? pathish?) . -> . hash?)
  (define source-path (->source-path (cond
                                       [(pagenode? pagenode-or-path) (pagenode->path pagenode-or-path)]
                                       [else pagenode-or-path])))
  (if source-path
      (cached-require source-path world:meta-pollen-export)
      (error (format "get-metas: no source found for '~a' in directory ~a" pagenode-or-path (current-directory)))))


(define (get-doc pagenode-or-path)
  ;  ((or/c pagenode? pathish?) . -> . (or/c txexpr? string?))
  (define source-path (->source-path (cond
                                       [(pagenode? pagenode-or-path) (pagenode->path pagenode-or-path)]
                                       [else pagenode-or-path])))
  (if source-path
      (cached-require source-path world:main-pollen-export)
      (error (format "get-doc: no source found for '~a' in directory ~a" pagenode-or-path (current-directory)))))


(define (trim-outer-tag html)
  (define matches (regexp-match #px"<.*?>(.*)</.*?>" html))
  (define paren-match (cadr matches))
  paren-match)

(define+provide/contract (->html x #:tag [tag #f] #:attrs [attrs #f] #:splice [splice? #f])
  ((xexpr?) (#:tag (or/c #f txexpr-tag?) #:attrs (or/c #f txexpr-attrs?) #:splice boolean?) . ->* . string?)
  
  (when (and (not (txexpr? x)) attrs (not tag))
    (error '->html "can't use attribute list '~a without a #:tag argument" attrs))
  
  (if (or tag (txexpr? x))
      (let ()
        (define html-tag (or tag (get-tag x)))
        (define html-attrs (or attrs (and (txexpr? x) (get-attrs x)) null))
        (define html-elements (or (and (txexpr? x) (get-elements x)) (list x)))
        (define html (xexpr->html (make-txexpr html-tag html-attrs html-elements)))
        (if splice?
            (trim-outer-tag html)
            html))
      (xexpr->html x)))

(module-test-external
 (define tx '(root (p "hello")))
 (check-equal? (->html tx) "<root><p>hello</p></root>")
 (check-equal? (->html #:tag 'brennan tx) "<brennan><p>hello</p></brennan>")
 (check-equal? (->html #:attrs '((id "dale")) tx) "<root id=\"dale\"><p>hello</p></root>")
 (check-equal? (->html #:splice #t tx) "<p>hello</p>")
 (check-equal? (->html #:tag 'brennan #:attrs '((id "dale")) tx) "<brennan id=\"dale\"><p>hello</p></brennan>")
 (check-equal? (->html #:tag 'brennan #:attrs '((id "dale")) #:splice #t tx) "<p>hello</p>")
 (define x "hello")
 (check-equal? (->html x) "hello")
 (check-equal? (->html #:tag 'brennan x) "<brennan>hello</brennan>")
 (check-exn exn:fail? (λ() (->html #:attrs '((id "dale")) x) "hello")) ;; won't work without tag
 (check-equal? (->html #:splice #t x) "hello")
 (check-equal? (->html #:tag 'brennan #:attrs '((id "dale")) x) "<brennan id=\"dale\">hello</brennan>")
 (check-equal? (->html #:tag 'brennan #:attrs '((id "dale")) #:splice #t x) "hello"))

(provide when/block)
(define-syntax (when/block stx)
  (syntax-case stx ()
    [(_ condition body ...)
     #'(if condition (string-append* 
                      (with-handlers ([exn:fail? (λ(exn) (error (format "within when/block, ~a" (exn-message exn))))])
                        (map ->string (list body ...)))) 
           "")]))