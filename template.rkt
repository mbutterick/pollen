#lang racket/base
(require racket/list racket/string xml xml/path)
(require "readability.rkt" "debug.rkt" "predicates.rkt" "tools.rkt")

;; setup for test cases
(module+ test (require rackunit))

(provide (all-defined-out))

;; todo: better fallback template

(define fallback-template-data "FALLBACK! ◊(put-as-html main)")

;; todo: tests & contracts for this subsection

(define (from x query)
  ; cache x
  (let ([x (put x)])
    ; try finding it in metas, if not, find it in main, if not then return false
    (or (from-metas x query) (from-main x query))))

(define (from-main x query) ; this used to be plain from
  ; check results first
  (let* ([x (put x)]
         [results (se-path*/list (list query) x)])
    ; if results exist, send back xexpr as output
    (if (not (empty? results))
        `(,query ,@results) ; todo: why use query as tag?
        #f)))

(define (from-metas x key)
  (let* ([x (put x)]
         [meta-hash (make-meta-hash x)]
         [key (->symbol key)])
    (if (in? meta-hash key)
        `(value ,(get meta-hash key)) ;todo: why use value as tag?
        #f)))

(define (put x)
  ; handles either xexpr or pollen file as input
  (cond
    ; pass through xexpr as is
    ; put is optional for xexprs.
    ; it's only here to make the idiom smooth.
    [(tagged-xexpr? x) x] 
    ; todo: how to externalize pollen main tag into world name?
    [(file-exists? (->path x)) (dynamic-require x 'main)]
    ; also try adding pollen file extension
    ; this makes put compatible with map references
    [(let ([x (make-pollen-source-path x)])
       (when (file-exists? x)
         (put x)))]
    [else (error "put: need named xexpr or pollen file, but got" x)]))


(define (merge x)
  (cond
    [(tagged-xexpr? x)
     ; return content of xexpr.
     ; pollen language rules will splice these into the main flow.
     (if (empty? x)
         ""
         (let-values([(name attr content) (break-tagged-xexpr x)])
           content))]
    [(string? x) (list x)]))


#|(define (merge-strings x)
  (when (empty? x) (error "merge-strings got empty x"))
  ;todo: filter metas?
  ; leaning toward no. Simplest behavior.
  ; function is not intended to be used with whole pollen body anyhow.
  (let ([x (merge x)])
    (string-join (filter string? (flatten x)) " ")))|#

(define (merge-strings x)
  (string-join (filter string? (flatten x)) " "))


(define (make-html x)
  (if (tagged-xexpr? x)
      (xexpr->string x)
      (let ([x (->list x)])
        (when (andmap xexpr? x)
          (string-join (map xexpr->string x) "")))))

; generate *-as-html versions of functions
(define-values (put-as-html merge-as-html merge-strings-as-html)
  (apply values (map (λ(proc) (λ(x) (make-html (proc x)))) (list put merge merge-strings))))
