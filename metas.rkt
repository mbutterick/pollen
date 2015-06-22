#lang racket/base
(require racket/list pollen/top txexpr pollen/world) ; pollen/top needed for metaroot
(provide split-metas-to-hash)

(require sugar)

(module+ test
  (require rackunit))


(define (possible-meta-element? x)
  (and (txexpr? x) (equal? world:meta-tag-name (get-tag x))))


(define (trivial-meta-element? x)
  (and (possible-meta-element? x) (not (nontrivial-meta-element? x))))


(define (has-meta-attrs x)
  (let ([attrs (get-attrs x)])
    (and (not (empty? attrs)) (andmap valid-meta-attr? attrs))))


(define (has-meta-elements x)
  (not (empty? (filter txexpr? (get-elements x)))))


(define (nontrivial-meta-element? x) 
  (and (possible-meta-element? x)
       (or (has-meta-attrs x) (has-meta-elements x))))


(define (meta-element? x)
  (or (trivial-meta-element? x) (nontrivial-meta-element? x)))


(module+ test
  (check-true (nontrivial-meta-element? '(meta ((foo "bar")))))
  (check-true (nontrivial-meta-element? '(meta (foo "bar"))))
  (check-true (trivial-meta-element? '(meta)))
  (check-true (trivial-meta-element? '(meta "bar"))))


;; strictly speaking, this predicate isn't necessary (implied by txexpr-ness)
;; but it produces a helpful error
(define (valid-meta-attr? x)
  (or (and (list? x) (symbol? (first x)) (string? (second x)))
      (error 'is-meta-element? "error: meta must be a symbol / string pair, instead got: ~v" x)))

;; all metas are converted into "atomic meta" format
;; which is '(meta (key value ...))
(define (make-atomic-meta key . values)
  `(meta (,key ,@values)))


(define (explode-meta-element me)
  ;; convert a meta with multiple key/value pairs into multiple metas with a single txexpr element
  ;; only gets nontrivial metas to start.
  (let loop ([me (make-txexpr (get-tag me) (get-attrs me) (filter txexpr? (get-elements me)))][acc empty])
    (cond
      [(not (trivial-meta-element? me)) ; meta might become trivial during loop
       (cond
         [(has-meta-attrs me) ; might have txexpr elements, so preserve them
          (define attrs (get-attrs me))
          (loop (make-txexpr 'meta (cdr attrs) (get-elements me)) (cons (apply make-atomic-meta (car attrs)) acc))]
         [else ; has txexpr elements, but not meta-attrs
          (define txexpr-elements (get-elements me)) ; elements were filtered for txexpr at loop entry
          (loop (make-txexpr 'meta null (cdr txexpr-elements)) (cons (apply make-atomic-meta (car txexpr-elements)) acc))])]
      [else (reverse acc)])))


(define (split-meta-elements x) ; pull metas out of doc and put them into meta-elements accumulator
  (when (not (txexpr? x))
    (error 'split-meta-elements "Not a txexpr: ~v" x))
  (define-values (thing-without-meta-elements meta-elements) (splitf-txexpr x meta-element?))
  ;; trivial metas are discarded
  (define exploded-meta-elements (append-map explode-meta-element (filter nontrivial-meta-element? meta-elements)))
  (values thing-without-meta-elements exploded-meta-elements))


(define (split-metas-to-hash x)
  (define-values (doc-without-metas meta-elements) (split-meta-elements x))
  ;; 'metaroot is the hook for the meta decoder function.
  ;; If it's not a defined identifier, it just hits #%top and becomes `(metaroot ,@metas ...)
  ;; because of `explode-meta-element`, meta-elements will be a list of metas with a single key/value pair
  ;; metaroot can rely on this
  (define metas-xexpr (apply metaroot meta-elements))
  (define (first-attribute x) (car (get-elements x)))
  (define (meta-key x) (car (first-attribute x)))
  (define (meta-value x) (let ([rest (cdr (first-attribute x))])
                           (if (= (length rest) 1)
                               (car rest)
                               rest)))
  (define (meta-element->assoc me) (cons (meta-key me) (meta-value me)))
  (define metas (make-hash (map meta-element->assoc (cdr metas-xexpr))))
  (values doc-without-metas metas))


(module+ test
  (require rackunit)
  (let ([x '(root (meta ((foo "bar"))) "hello" (p (meta ((foo "zam"))) (meta) "there"))])
    (define-values (doc-without-metas metahash) (split-metas-to-hash x))
    (check-equal? doc-without-metas '(root "hello" (p "there")))
    (check-equal? (hash-ref metahash 'foo) "zam"))
  
  (let ([x '(root (meta (foo "bar")) "hello" (p (meta (foo (zim "zam"))) (meta) "there"))])
    (define-values (doc-without-metas metahash) (split-metas-to-hash x))
    (check-equal? doc-without-metas '(root "hello" (p "there")))
    (check-equal? (hash-ref metahash 'foo) '(zim "zam"))))