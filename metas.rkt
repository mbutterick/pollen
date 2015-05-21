#lang racket/base
(require racket/list pollen/top txexpr) ; pollen/top needed for metaroot
(provide split-metas-to-hash)

(define (meta-element? x)
  (or (trivial-meta-element? x) (nontrivial-meta-element? x)))

(define (trivial-meta-element? x) ; trivial meta has no attributes.
  (and (possible-meta-element? x) (empty? (get-attrs x))))

(define (nontrivial-meta-element? x) ; nontrivial meta has attributes that are valid.
  (and (possible-meta-element? x)
       (let ([attrs (get-attrs x)])
         (and (not (empty? attrs)) (andmap valid-meta-attr? attrs)))))

(define (possible-meta-element? x)
  (and (txexpr? x) (equal? 'meta (get-tag x))))

(define (valid-meta-attr? x)
  (or (and (list? x) (symbol? (first x)) (string? (second x)))
      (error 'is-meta-element? "error: meta must be a symbol / string pair, instead got: ~v" x)))

(define (explode-meta-element me)
  ;; convert a meta with multiple key/value pairs into multiple metas with a single key/value pair
  ;; only gets nontrivial metas to start.
  (let loop ([me me][acc empty])
    (cond
      [(not (trivial-meta-element? me)) ; meta might become trivial during loop (no attrs)
       (define attrs (get-attrs me))
       (loop `(meta ,(cdr attrs)) (cons `(meta ,(list (car attrs))) acc))]
      [else (reverse acc)])))

(define (split-meta-elements x) ; pull metas out of doc and put them into meta-elements accumulator
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
  (define (first-attribute x) (car (get-attrs x)))
  (define (meta-key x) (first (first-attribute x)))
  (define (meta-value x) (second (first-attribute x)))
  (define (meta-element->assoc me) (cons (meta-key me) (meta-value me)))
  (define metas (make-hash (map meta-element->assoc (cdr metas-xexpr))))
  (values doc-without-metas metas))


(module+ test
  (require rackunit)
  (define x '(root (meta ((foo "bar"))) "hello" (p (meta ((foo "zam"))) (meta) "there")))
  (define-values (doc-without-metas metahash) (split-metas-to-hash x))
  (check-equal? doc-without-metas '(root "hello" (p "there")))
  (check-equal? (hash-ref metahash 'foo) "zam"))