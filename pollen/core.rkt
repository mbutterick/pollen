#lang racket/base
(require (for-syntax racket/base "world.rkt"))
(require txexpr xml/path sugar/define sugar/coerce sugar/test racket/string)
(require "private/file-utils.rkt"
         "world.rkt"
         "cache.rkt"
         "pagetree.rkt"
         "private/to-string.rkt")

(define is-meta-value? hash?)
(define is-doc-value? txexpr?)
(define identity (λ(x) x))
(define not-false? identity)

(define+provide define-meta identity) ;; stub so it will be picked up for docs


(define+provide/contract (select* key value-source)
  (coerce/symbol? (or/c is-meta-value? is-doc-value? pagenode? pathish?) . -> . (or/c #f txexpr-elements?))
  (define metas-result (and (not (is-doc-value? value-source)) (select-from-metas key value-source)))
  (define doc-result  (and (not (is-meta-value? value-source)) (select-from-doc key value-source)))
  (define result (filter not-false? (apply append (map ->list (list metas-result doc-result)))))
  (and (pair? result) result))


(define+provide/contract (select key value-source)
  (coerce/symbol? (or/c is-meta-value? is-doc-value? pagenode? pathish?) . -> . (or/c #f txexpr-element?))
  (define result (select* key value-source))
  (and (pair? result) (car result)))


(module-test-external
 (check-equal? (select* 'key  '#hash((key . "value"))) '("value"))
 (check-equal? (select 'key  '#hash((key . "value"))) "value")
 (check-false (select* 'absent-key  '#hash((key . "value"))))
 (check-false (select 'absent-key  '#hash((key . "value"))))
 (check-equal? (select* 'key  '(root (key "value"))) '("value"))
 (check-equal? (select 'key  '(root (key "value"))) "value")
 (check-false (select* 'absent-key  '(root (key "value"))))
 (check-false (select 'absent-key  '(root (key "value"))))
 (let ([metas '#hash((key . "value"))])
   (check-equal? (select* 'key  metas) '("value"))
   (check-equal? (select 'key  metas) "value")
   (check-false (select* 'absent-key  metas))
   (check-false (select 'absent-key  metas)))
 (let ([doc '(root (key "value"))])
   (check-equal? (select* 'key doc) '("value"))
   (check-equal? (select 'key doc) "value")
   (check-false (select* 'absent-key  doc))
   (check-false (select 'absent-key  doc))))


(define+provide/contract (select-from-metas key metas-source)
  ;; output contract is a single txexpr-element
  ;; because metas is a hash, and a hash has only one value for a key.
  (coerce/symbol? (or/c is-meta-value? pagenode? pathish?) . -> . (or/c #f txexpr-element?))
  (define metas (if (is-meta-value? metas-source)
                    metas-source
                    (get-metas metas-source)))
  (and (hash-has-key? metas key) (hash-ref metas key)))

(module-test-external
 (let ([metas '#hash((key . "value"))])
   (check-equal? (select-from-metas 'key  metas) "value")
   (check-false (select-from-metas 'absent-key  metas))))


(define+provide/contract (select-from-doc key doc-source)
  ;; output contract is a list of elements
  ;; because doc is a txexpr, and a txexpr can have multiple values for a key
  (coerce/symbol? (or/c is-doc-value? pagenode? pathish?) . -> . (or/c #f txexpr-elements?))
  (define doc (if (is-doc-value? doc-source)
                  doc-source
                  (get-doc doc-source)))
  (define result (se-path*/list (list key) doc))
  (and (pair? result) result))

(module-test-external
 (check-equal? (select-from-doc 'key  '(root (key "value"))) '("value"))
 (check-false (select-from-doc 'absent-key  '(root (key "value"))))
 (let ([doc '(root (key "value"))])
   (check-equal? (select-from-doc 'key  doc) '("value"))
   (check-false (select-from-doc 'absent-key  doc))))


(define (convert+validate-path pagenode-or-path caller)
  (let ([path (get-source (if (pagenode? pagenode-or-path)
                              (build-path (world:current-project-root) (symbol->string pagenode-or-path))
                              pagenode-or-path))])
    (unless path
      (error (format "~a no source found for '~a' in directory ~a" caller path (current-directory))))
    path))


(define+provide/contract (get-metas pagenode-or-path)
  ((or/c pagenode? pathish?) . -> . is-meta-value?) 
  (cached-metas (convert+validate-path pagenode-or-path 'get-metas)))


(define+provide/contract (get-doc pagenode-or-path)
  ((or/c pagenode? pathish?) . -> . (or/c is-doc-value? string?))
  (cached-doc (convert+validate-path pagenode-or-path 'get-doc)))


(provide when/splice)
(define-syntax (when/splice stx)
  (syntax-case stx ()
    [(_ COND BODY ...)
     (with-syntax ([SPLICING-TAG (datum->syntax stx (world:current-splicing-tag))])
       #'(if COND
             (with-handlers ([exn:fail? (λ(exn) (error (format "within when/block, ~a" (exn-message exn))))])
               (list 'SPLICING-TAG BODY ...)) 
             ""))]))

(provide when/block) ; bw compat
(define-syntax (when/block stx)
  (syntax-case stx ()
    [(_ condition body ...)
     #'(if condition (string-append* 
                      (with-handlers ([exn:fail? (λ(exn) (error (format "within when/block, ~a" (exn-message exn))))])
                        (map to-string (list body ...)))) 
           "")]))


