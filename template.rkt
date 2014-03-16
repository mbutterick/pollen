#lang racket/base
(require (for-syntax racket/base))
(require racket/string xml xml/path sugar/define sugar/container sugar/coerce/contract)
(require "tools.rkt" txexpr "world.rkt" "cache.rkt" "pagetree.rkt")


(require sugar/coerce/value)
(provide (all-from-out sugar/coerce/value))


(define/contract+provide (metas->here metas)
  (hash? . -> . pagenode?)
  (path->pagenode ('here-path . from-metas . metas)))


(define/contract (get-doc pagenode-or-path)
  ((or/c pagenode? pathish?) . -> . (or/c txexpr? string?))
  (define source-path (->source-path (cond
                                       [(pagenode? pagenode-or-path) (pagenode->path pagenode-or-path)]
                                       [else pagenode-or-path])))
  (if source-path
      (cached-require source-path world:main-pollen-export)
      (error (format "get-doc: no source found for '~a' in directory ~a" pagenode-or-path (current-directory)))))


(define/contract (get-metas pagenode-or-path)
  ((or/c pagenode? pathish?) . -> . hash?)
  (define source-path (->source-path (cond
                                       [(pagenode? pagenode-or-path) (pagenode->path pagenode-or-path)]
                                       [else pagenode-or-path])))
  (if source-path
      (cached-require source-path world:meta-pollen-export)
      (error (format "get-metas: no source found for '~a' in directory ~a" pagenode-or-path (current-directory)))))


(define (pagenode->path pagenode)
  (build-path (world:current-project-root) (symbol->string pagenode)))


(define+provide/contract (from query pagenode)
  (coerce/symbol? coerce/symbol? . -> . (or/c #f txexpr-element?))
  (define result (from* query pagenode))
  (if (null? result) #f (car result)))


(define+provide/contract (from* query pagenode)
  (coerce/symbol? coerce/symbol? . -> . (or/c #f (listof txexpr-element?)))
  (define meta-result (from-metas query pagenode))
  (define doc-result (from-doc query pagenode))
  (define combined-result (append (if meta-result (list meta-result) null) 
          (or doc-result null)))
  (if (null? combined-result) #f combined-result))


(define/contract+provide (from-metas query meta-source)
  (coerce/symbol? (or/c pagenode? hash?) . -> . (or/c #f txexpr-element?))
  (let ([metas (or (and (pagenode? meta-source) (get-metas meta-source)) meta-source)])
    (with-handlers ([exn:fail? (λ(e) #f)])
      (hash-ref metas query))))


(define/contract+provide (from-doc query doc-source) 
  (coerce/symbol? (or/c pagenode? txexpr?) . -> . (or/c #f txexpr-elements?))
  (let ([doc (or (and (pagenode? doc-source) (get-doc doc-source)) doc-source)])
    (with-handlers ([exn:fail? (λ(e) #f)])
      (se-path*/list (list query) doc))))


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

