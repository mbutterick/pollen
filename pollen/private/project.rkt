#lang racket/base
(require racket/syntax
         sugar/define
         sugar/coerce
         "../setup.rkt"
         "file-utils.rkt")

(define+provide/contract (get-directory-require-files source-arg)
  (pathish? . -> . (or/c #f (λ (xs) (and (list? xs) (andmap complete-path? xs)))))
  (define source-path (->path source-arg))  
  (define require-filenames (list default-directory-require))
  (define possible-requires (for*/list ([rf (in-list require-filenames)]
                                        [p (in-value (find-upward-from source-path rf))]
                                        #:when p)
                                       p))
  (and (pair? possible-requires) possible-requires))


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