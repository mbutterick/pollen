#lang racket/base
(require (for-syntax racket/base "setup.rkt" "private/splice.rkt"))
(require txexpr/base xml/path sugar/define sugar/coerce sugar/test racket/string)
(require "private/file-utils.rkt"
         "setup.rkt"
         "cache.rkt"
         "pagetree.rkt"
         "tag.rkt"
         "private/splice.rkt")

(define is-meta-value? hash?)
(define is-doc-value? txexpr?)

;; if `define-meta` is defined it will pop an error msg if the wrong number of args
;; even though this error will happen after macro expansion, when metas are extracted
;; empty string will merge with surroundings
(define+provide (define-meta k v) "")

(define+provide current-metas (make-parameter #f))

(define+provide/contract (select* key value-source [caller 'select*])
  ((coerce/symbol? (or/c is-meta-value? is-doc-value? pagenode? pathish?)) (symbol?) . ->* . (or/c #f txexpr-elements?))
  (define metas-result (and (not (is-doc-value? value-source)) (select-from-metas key value-source caller)))
  (define doc-result  (and (not (is-meta-value? value-source)) (select-from-doc key value-source caller)))
  (define result (filter values (apply append (map ->list (list metas-result doc-result)))))
  (and (pair? result) result))


(define+provide/contract (select key value-source)
  (coerce/symbol? (or/c is-meta-value? is-doc-value? pagenode? pathish?) . -> . (or/c #f txexpr-element?))
  (define result (select* key value-source 'select))
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


(define+provide/contract (select-from-metas key metas-source [caller 'select-from-metas])
  ;; output contract is a single txexpr-element
  ;; because metas is a hash, and a hash has only one value for a key.
  ((coerce/symbol? (or/c is-meta-value? pagenode? pathish?)) (symbol?) . ->* . (or/c #f txexpr-element?))
  (define metas (if (is-meta-value? metas-source)
                    metas-source
                    (get-metas metas-source caller)))
  (and (hash-has-key? metas key) (hash-ref metas key)))

(module-test-external
 (let ([metas '#hash((key . "value"))])
   (check-equal? (select-from-metas 'key  metas) "value")
   (check-false (select-from-metas 'absent-key  metas))))


(define+provide/contract (select-from-doc key doc-source [caller 'select-from-doc])
  ;; output contract is a list of elements
  ;; because doc is a txexpr, and a txexpr can have multiple values for a key
  ((coerce/symbol? (or/c is-doc-value? pagenode? pathish?)) (symbol?) . ->* . (or/c #f txexpr-elements?))
  (define doc (if (is-doc-value? doc-source)
                  doc-source
                  (get-doc doc-source caller)))
  (define result (se-path*/list (list key) doc))
  (and (pair? result) result))

(module-test-external
 (check-equal? (select-from-doc 'key  '(root (key "value"))) '("value"))
 (check-false (select-from-doc 'absent-key  '(root (key "value"))))
 (let ([doc '(root (key "value"))])
   (check-equal? (select-from-doc 'key  doc) '("value"))
   (check-false (select-from-doc 'absent-key  doc))))


(define (convert+validate-path pagenode-or-path caller)
  (let* ([path (if (pagenode? pagenode-or-path)
                   (build-path (current-project-root) (symbol->string pagenode-or-path))
                   pagenode-or-path)]
         [path (or (get-source path) path)])
    (unless (file-exists? path)
      (raise-argument-error caller "existing Pollen source, or name of its output path" path))
    path))


(define+provide/contract (get-metas pagenode-or-path [caller 'get-metas])
  (((or/c pagenode? pathish?)) (symbol?) . ->* . is-meta-value?) 
  (cached-metas (convert+validate-path pagenode-or-path caller)))


(define+provide/contract (get-doc pagenode-or-path [caller 'get-doc])
  (((or/c pagenode? pathish?)) (symbol?) . ->* . (or/c is-doc-value? string?))
  (cached-doc (convert+validate-path pagenode-or-path caller)))


;; This `@` definition is here to provide a hook for the docs.
;; But this is just default tag behavior, and thus would work without the definition.
;; Which is why the splicing tag can be renamed:
;; it just becomes an undefined tag, also with default behavior.
;; For a pollen source, the actual splicing happens when the source is compiled.
;; For a template in the render environment, which is more text-ish,
;; the splicing tag is redefined to produce a basic list.
;; (which is then, by default behavior of `include-template`, merged with the surrounding text)
(define+provide @ (make-default-tag-function '@))

(provide when/splice)
(define-syntax (when/splice stx)
  (syntax-case stx ()
    [(_ COND . BODY)
     (with-syntax ([SPLICING-TAG (datum->syntax stx (setup:splicing-tag))])
       #'(if COND
             (SPLICING-TAG . BODY) 
             (SPLICING-TAG)))]))


(provide for/splice for*/splice)

(define-syntax (for/splice/base stx)
  (syntax-case stx ()
    [(_ ITERATORS . BODY)
     (with-syntax ([SPLICING-TAG (datum->syntax stx (setup:splicing-tag))]
                   [FORM (or (syntax-property stx 'form) #'for/list)])
       #'(apply SPLICING-TAG (FORM ITERATORS
                                   (SPLICING-TAG . BODY))))]))

(define-syntax for/splice (make-rename-transformer #'for/splice/base))
(define-syntax (for*/splice stx)
  (syntax-case stx ()
    [(_ . BODY) (syntax-property #'(for/splice/base . BODY) 'form #'for*/list)]))


(provide when/block) ; bw compat
(define-syntax when/block (make-rename-transformer #'when/splice))