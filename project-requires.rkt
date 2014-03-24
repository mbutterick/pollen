#lang racket/base
(require "world.rkt" sugar/define sugar/coerce/contract)


(define/contract+provide (get-project-require-files source-path) ; keep contract local to ensure coercion
  (coerce/path? . -> . (or/c #f (listof path?)))
  (define possible-requires (list (simplify-path (build-path source-path 'up world:project-require))))
  (and (andmap file-exists? possible-requires) possible-requires))


(define+provide/contract (require+provide-project-require-files here-path #:provide [provide #t])
  (coerce/path? . -> . (or/c list? void?))
  (define (put-file-in-require-form file)
    `(file ,(path->string file)))
  (define project-require-files (get-project-require-files here-path))
  
  (if project-require-files
      (let ([files-in-require-form (map put-file-in-require-form project-require-files)])
        `(begin
           (require ,@files-in-require-form)
           ,@(if provide
                 (list `(provide (all-from-out ,@files-in-require-form)))
                 '())))
      '(begin)))


(define+provide (require-project-require-files here-path)
  (require+provide-project-require-files here-path #:provide #f))
