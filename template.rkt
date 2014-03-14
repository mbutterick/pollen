#lang racket/base
(require (for-syntax racket/base))
(require racket/string xml xml/path sugar/define sugar/container sugar/coerce/contract)
(require "tools.rkt" txexpr "world.rkt" "cache.rkt")


(require sugar/coerce/value)
(provide (all-from-out sugar/coerce/value))


(define/contract+provide (get-doc x)
  (coerce/path? . -> . (or/c #f txexpr? string?))
  (define source-path (->source-path x))
  (if source-path
      (cached-require source-path world:main-pollen-export)
      (error (format "get-doc: no source found for '~a' in directory ~a" x (current-directory)))))


(define/contract+provide (get-metas x)
  (coerce/path? . -> . hash?)
  (define source-path (->source-path x))
  (if source-path
      (cached-require source-path world:meta-pollen-export)
      (error (format "get-doc: no source found for '~a' in directory ~a" x (current-directory)))))


(define/contract+provide (from-node query node)
  (coerce/symbol? coerce/symbol? . -> . (or/c #f txexpr-element?))
  (define node-path (build-path (world:current-project-root) (->string node)))
  (define result (append (find-in-metas query node-path) (find-in-doc query node-path)))
  (if (null? result) #f (car result)))


(define/contract+provide (find* query . nodes)
  ((coerce/symbol?) #:rest (listof symbol?) . ->* . (or/c #f txexpr-elements?))
  (define (finder x)
    (cond
      [(hash? x) (find-in-metas query x)]
      [(txexpr? x) (find-in-doc query x)]
      [(pathish? x) (find* query (get-doc x) (get-metas x))]
      [else null]))
  (append-map finder nodes))


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
  (xexpr? . -> . string?)  
  (xexpr->html x))


(provide when/block)
(define-syntax (when/block stx)
  (syntax-case stx ()
    [(_ condition body ...)
     #'(if condition (string-append* 
                      (with-handlers ([exn:fail? (λ(exn) (error (format "within when/block, ~a" (exn-message exn))))])
                        (map ->string (list body ...)))) 
           "")]))


(module+ main
  (parameterize ([current-directory (string->path "/Users/MB/git/bpt/down/")])
    (get-doc "introduction.html")))