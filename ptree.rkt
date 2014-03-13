#lang racket/base
(require "tools.rkt" "world.rkt" "decode.rkt" sugar txexpr "cache.rkt")


(define+provide current-ptree (make-parameter #f))


(define+provide (pnode? x)
  (->boolean (and (symbol? x) (try (not (whitespace/nbsp? (->string x)))
                                   (except [exn:fail? (λ(e) #f)])))))


(define+provide (pnodeish? x)
  (try (pnode? (->symbol x))
       (except [exn:fail? (λ(e) #f)])))


(define/contract+provide (->pnode x)
  (pnodeish? . -> . pnode?)
  (->symbol x))


(define+provide/contract (decode-ptree xs)
  (txexpr-elements? . -> . any/c) ; because ptree is being explicitly validated
  (validate-ptree 
   (decode (cons world:ptree-root-node xs)
           #:txexpr-elements-proc (λ(xs) (filter (compose1 not whitespace?) xs))
           #:string-proc string->symbol))) ; because faster than ->pnode


(define+provide (validate-ptree x)
  (let ([pnodes (ptree->list x)])
    (and
     (andmap (λ(p) (or (pnode? p) (error (format "validate-ptree: \"~a\" is not a valid pnode" p)))) pnodes)
     (try (members-unique?/error pnodes)
          (except [exn:fail? (λ(e) (error (format "validate-ptree: ~a" (exn-message e))))]))
     x)))


(define+provide (ptree? x)
  (try (->boolean (validate-ptree x))
       (except [exn:fail? (λ(e) #f)])))


;; Try loading from ptree file, or failing that, synthesize ptree.
(define+provide/contract (make-project-ptree project-dir)
  (pathish? . -> . ptree?)
  (define ptree-source (build-path project-dir world:default-ptree))
  (cached-require ptree-source world:main-pollen-export))


(define+provide/contract (parent p [ptree (current-ptree)])
  (((or/c #f pnodeish?)) (ptree?) . ->* . (or/c #f pnode?)) 
  (and ptree p
       (let ([pnode (->pnode p)])
         (if (member pnode (map (λ(x) (if (list? x) (car x) x)) (cdr ptree)))
             (car ptree)
             (ormap (λ(x) (parent pnode x)) (filter list? ptree))))))


(define+provide/contract (children p [ptree (current-ptree)])
  (((or/c #f pnodeish?)) (ptree?) . ->* . (or/c #f (listof pnode?)))  
  (and ptree p 
       (let ([pnode (->pnode p)])
         (if (equal? pnode (car ptree))
             (map (λ(x) (if (list? x) (car x) x)) (cdr ptree))
             (ormap (λ(x) (children pnode x)) (filter list? ptree))))))


(define+provide/contract (siblings p [ptree (current-ptree)])
  (((or/c #f pnodeish?)) (ptree?) . ->* . (or/c #f (listof pnode?)))  
  (children (parent p ptree) ptree))


;; flatten tree to sequence
(define+provide/contract (ptree->list ptree)
  (ptree? . -> . (listof pnode?))
  ; use cdr to get rid of root tag at front
  (cdr (flatten ptree))) 


(define (adjacents side p [ptree (current-ptree)])
  ; ((symbol? (or/c #f pnodeish?)) (ptree?) . ->* . (or/c #f (listof pnode?)))
  (and ptree p
       (let* ([pnode (->pnode p)]
              [proc (if (equal? side 'left) takef takef-right)]
              [result (proc (ptree->list ptree) (λ(x) (not (equal? pnode x))))])
         (and (not (empty? result)) result))))


(define+provide/contract (previous* pnode [ptree (current-ptree)]) 
  (((or/c #f pnodeish?)) (ptree?) . ->* . (or/c #f (listof pnode?)))
  (adjacents 'left pnode ptree))


(define+provide/contract (next* pnode [ptree (current-ptree)]) 
  (((or/c #f pnodeish?)) (ptree?) . ->* . (or/c #f (listof pnode?)))
  (adjacents 'right pnode ptree))


(define+provide/contract (previous pnode [ptree (current-ptree)])
  (((or/c #f pnodeish?)) (ptree?) . ->* . (or/c #f pnode?))
  (let ([result (previous* pnode ptree)])
    (and result (last result))))


(define+provide/contract (next pnode [ptree (current-ptree)])
  (((or/c #f pnodeish?)) (ptree?) . ->* . (or/c #f pnode?))
  (let ([result (next* pnode ptree)])
    (and result (first result))))


