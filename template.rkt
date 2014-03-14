#lang racket/base
(require (for-syntax racket/base))
(require racket/string xml xml/path sugar/define sugar/container sugar/coerce/contract)
(require "tools.rkt" txexpr "world.rkt" "cache.rkt")


(require sugar/coerce/value)
(provide (all-from-out sugar/coerce/value))


(define/contract+provide (get-doc x)
  (coerce/path? . -> . txexpr?)
  (cached-require (->source-path x) world:main-pollen-export))


(define/contract+provide (get-metas x)
  (coerce/path? . -> . hash?)
  (cached-require (->source-path x) world:meta-pollen-export))


(define/contract+provide (find query . xs)
  ((coerce/symbol?) #:rest (listof (or/c #f hash? txexpr? pathish?)) . ->* . (or/c #f txexpr-element?))
  (define result (apply find* query xs))
  (or (null? result) (car result)))


(define/contract+provide (find* query . pxs)
  ((coerce/symbol?) #:rest (listof (or/c #f hash? txexpr? pathish?)) . ->* . (or/c #f txexpr-elements?))
  (define (finder x)
    (cond
      [(hash? x) (find-in-metas query x)]
      [(txexpr? x) (find-in-doc query x)]
      [(pathish? x) (find* query (get-doc x) (get-metas x))]
      [else null]))
  (append-map finder pxs))


(define/contract+provide (find-in-metas query hash-or-path)
  (coerce/symbol? (or/c hash? pathish?) . -> . (or/c #f txexpr-elements?))
  (let ([metas (or (and (hash? hash-or-path) hash-or-path) 
                   (get-metas (->path hash-or-path)))])
    (with-handlers ([exn:fail? (λ(e) null)])
      (list (hash-ref metas query)))))


(define/contract+provide (find-in-doc query doc-or-path) 
  (coerce/symbol? (or/c txexpr? pathish?) . -> . (or/c  #f txexpr-elements?))
  (let ([doc (or (and (txexpr? doc-or-path) doc-or-path) 
                 (get-doc (->path doc-or-path)))])
    (with-handlers ([exn:fail? (λ(e) null)])
      (se-path*/list (list query) doc))))


;; turns input into xexpr-elements so they can be spliced into template
;; (as opposed to dropped in as a full txexpr)
;; by returning a list, pollen rules will automatically merge into main flow
;; todo: explain why
;; todo: do I need this?
(define+provide/contract (splice x)
  ((or/c txexpr? txexpr-elements? string?) . -> . txexpr-elements?)
  (cond
    [(txexpr? x) (get-elements x)]
    [(txexpr-elements? x) x]
    [(string? x) (->list x)]))


(define+provide/contract (->html x)
  (txexpr? . -> . string?)
  (txexpr->html x))


(provide when/block)
(define-syntax (when/block stx)
  (syntax-case stx ()
    [(_ condition body ...)
     #'(if condition (string-append* 
                      (with-handlers ([exn:fail? (λ(exn) (error (format "when/block: ~a" (exn-message exn))))])
                        (map ->string (list body ...))))
           "")]))


(module+ main

  
(when/block #t (find 'topic "/Users/mb/git/bpt/introduction.html")))