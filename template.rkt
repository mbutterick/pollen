#lang racket/base
(require (for-syntax racket/base))
(require racket/string xml xml/path sugar/define sugar/container sugar/coerce/contract)
(require "tools.rkt" txexpr "world.rkt" "cache.rkt" "pagemap.rkt")


(require sugar/coerce/value)
(provide (all-from-out sugar/coerce/value))


(define/contract+provide (metas->here metas)
  (hash? . -> . node?)
  (path->node ('here-path . from-metas . metas)))


(define/contract (get-doc node-or-path)
  ((or/c node? pathish?) . -> . (or/c txexpr? string?))
  (define source-path (->source-path (cond
                                       [(node? node-or-path) (node->path node-or-path)]
                                       [else node-or-path])))
  (if source-path
      (cached-require source-path world:main-pollen-export)
      (error (format "get-doc: no source found for '~a' in directory ~a" node-or-path (current-directory)))))


(define/contract (get-metas node-or-path)
  ((or/c node? pathish?) . -> . hash?)
  (define source-path (->source-path (cond
                                       [(node? node-or-path) (node->path node-or-path)]
                                       [else node-or-path])))
  (if source-path
      (cached-require source-path world:meta-pollen-export)
      (error (format "get-metas: no source found for '~a' in directory ~a" node-or-path (current-directory)))))


(define (node->path node)
  (build-path (world:current-project-root) (symbol->string node)))


(define+provide/contract (from-node query node)
  (coerce/symbol? coerce/symbol? . -> . (or/c #f txexpr-element?))
  (define result (from-node* query node))
  (if (null? result) #f (car result)))


(define+provide/contract (from-node* query node)
  (coerce/symbol? coerce/symbol? . -> . (listof txexpr-element?))
  (define meta-result (from-metas query node))
  (append (if meta-result (list meta-result) null) (from-doc query node)))


(define/contract+provide (from-metas query node-or-metas)
  (coerce/symbol? (or/c node? hash?) . -> . (or/c #f txexpr-element?))
  (let ([metas (or (and (node? node-or-metas) (get-metas node-or-metas)) node-or-metas)])
    (with-handlers ([exn:fail? (λ(e) #f)])
      (hash-ref metas query))))


(define/contract+provide (from-doc query node-or-doc) 
  (coerce/symbol? (or/c node? txexpr?) . -> . (or/c  #f txexpr-elements?))
  (let ([doc (or (and (node? node-or-doc) (get-doc node-or-doc)) node-or-doc)])
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

