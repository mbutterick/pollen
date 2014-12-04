#lang racket/base
(require "world.rkt" sugar/define sugar/coerce)

(define (paths? x) (and (list? x) (andmap path? x)))

(define/contract+provide (get-directory-require-files source-path) ; keep contract local to ensure coercion
  (coerce/path? . -> . (or/c #f paths?))
  (define possible-requires (list (simplify-path (build-path source-path 'up world:directory-require))))
  (and (andmap file-exists? possible-requires) possible-requires))


(define+provide/contract (require+provide-directory-require-files here-path #:provide [provide #t])
  (coerce/path? . -> . (or/c list? void?))
  (define (put-file-in-require-form file)
    `(file ,(path->string file)))
  (define directory-require-files (get-directory-require-files here-path))
  
  (if directory-require-files
      (let ([files-in-require-form (map put-file-in-require-form directory-require-files)])
        `(begin
           (require ,@files-in-require-form)
           ,@(if provide
                 (list `(provide (all-from-out ,@files-in-require-form)))
                 '())))
      '(begin)))


(define+provide (require-directory-require-files here-path)
  (require+provide-directory-require-files here-path #:provide #f))