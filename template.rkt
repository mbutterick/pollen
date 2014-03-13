#lang racket/base
(require (for-syntax racket/base))
(require racket/string xml xml/path sugar/define sugar/container)
(require "tools.rkt" txexpr "world.rkt" "cache.rkt")


(require sugar/coerce/value)
(provide (all-from-out sugar/coerce/value))


(define+provide/contract (puttable-item? x)
  (any/c . -> . boolean?)
  (or (txexpr? x) (has-markup-source? x)))


(define+provide/contract (query-key? x)
  (any/c . -> . boolean?)
  (or (string? x) (symbol? x)))


(define+provide/contract (put x)
  (puttable-item? . -> . txexpr?)
  (cond
    ;; Using put has no effect on txexprs. It's here to make the idiom smooth.
    [(txexpr? x) x] 
    [(has-markup-source? x) (cached-require (->markup-source-path x) world:main-pollen-export)]))


(define+provide/contract (find query px)
  (query-key? (or/c #f puttable-item?) . -> . (or/c #f txexpr-element?))
  (define result (and px (or (find-in-metas px query) (find-in-doc px query))))
  (and result (car result))) ;; return false or first element


(define+provide/contract (find-in-metas px key)
  (puttable-item? query-key? . -> . (or/c #f txexpr-elements?))
  (and (has-markup-source? px)
       (let ([metas (cached-require (->markup-source-path px) 'metas)]
             [key (->string key)])
         (and (key . in? . metas ) (->list (get metas key))))))


(define+provide/contract (find-in-doc px query) 
  (puttable-item? (or/c query-key? (listof query-key?)) 
                  . -> . (or/c  #f txexpr-elements?))
  (let* ([px (put px)]
         ;; make sure query is a list of symbols (required by se-path*/list)
         [query (map ->symbol (->list query))]
         [results (se-path*/list query px)])
    ;; if results exist, send back xexpr as output
    (and (not (empty? results)) results)))


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
