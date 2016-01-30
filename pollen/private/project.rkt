#lang racket/base
(require sugar/define
         sugar/coerce
         "../setup.rkt"
         "file-utils.rkt")

(define+provide/contract (get-directory-require-files source-arg)
  (pathish? . -> . (or/c #f (λ(xs) (and (list? xs) (andmap complete-path? xs)))))
  (define source-path (->path source-arg))  
  (define require-filenames (list setup:default-directory-require))
  (define identity (λ(x) x))
  (define possible-requires (filter identity (map (λ(f) (find-upward-from source-path f)) require-filenames)))
  (and (pair? possible-requires) possible-requires))


(define+provide/contract (require+provide-directory-require-files here-arg #:provide [provide #t])
  (pathish? . -> . list?)
  (define here-path (->path here-arg))  
  (define (put-file-in-require-form file) `(file ,(path->string file)))  
  (define directory-require-files (get-directory-require-files here-path))
  (if directory-require-files
      (let ([files-in-require-form (map put-file-in-require-form directory-require-files)])
        `(begin
           (require ,@files-in-require-form)
           ,@(if provide
                 (list `(provide (all-from-out ,@files-in-require-form)))
                 null)))
      '(begin)))


(define+provide (require-directory-require-files here-path)
  (require+provide-directory-require-files here-path #:provide #f))