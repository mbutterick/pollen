#lang racket/base
(require racket/syntax
         racket/match
         sugar/define
         sugar/coerce
         "../setup.rkt"
         "file-utils.rkt")

(define+provide/contract (get-directory-require-files source-arg)
  (pathish? . -> . (or/c #false (λ (xs) (and (list? xs) (andmap complete-path? xs)))))
  ;; only one file, but we'll leave it in plural form
  (match (for*/list ([rf (in-list (list default-directory-require))]
                     [path (in-value (find-upward-from (->path source-arg) rf))]
                     #:when path)
                    path)
    [(? pair? possible-requires) possible-requires]
    [_ #false]))


(define+provide/contract (require+provide-directory-require-files here-arg #:provide [provide? #t])
  (pathish? . -> . syntax?)
  (define here-path (->path here-arg))  
  (with-syntax* ([(DRF ...) (map path->string (or (get-directory-require-files here-path) null))]
                 [(PROVIDE-DRF ...) (if provide? #'(DRF ...) #'())])
    #'(begin
        (require (file DRF)) ...
        (provide (all-from-out (file PROVIDE-DRF))) ...)))


(define+provide (require-directory-require-files here-path)
  (require+provide-directory-require-files here-path #:provide #f))