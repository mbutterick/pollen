#lang racket/base
(require (for-syntax racket/base))

(require racket/contract/region)

(provide (all-defined-out) (all-from-out racket/contract/region))

;; A place to stash functions that don't change between compiles of Pollen files.

;; duplicate of contents of project-require.rkt.
;; Goes faster if it's not in a separate module.
;; todo: use include? But this one has to be available as syntax
;; todo: get rid of magic value
(define-for-syntax (project-require-file? path)
  (define path-string (path->string path))
  (equal? (substring path-string (- (string-length path-string) 3) (string-length path-string)) "rkt"))

;; list of all eligible requires in project require directory
(define-for-syntax (get-project-require-files)
  (define extras-directory (build-path (current-directory) "pollen-require"))
  (and (directory-exists? extras-directory)
       ;; #:build? option returns complete paths (instead of just file names)
       (let ([files (filter project-require-file? (directory-list extras-directory #:build? #t))])
         (and (not (equal? '() files)) files))))

(define-for-syntax (put-file-in-require-form file)
  `(file ,(path->string file)))

(define-for-syntax (do-project-require-file-syntax stx #:provide? [provide? #f])
  (define project-require-files (get-project-require-files))
  (if project-require-files
      (let ([files-in-require-form (map put-file-in-require-form project-require-files)])
        (datum->syntax stx `(begin
                              (require ,@files-in-require-form)
                              ,@(if provide? 
                                    (list `(provide (all-from-out ,@files-in-require-form))) 
                                    '()))))
      ; if no files to import, do nothing
      #'(begin)))

(define-syntax (require-and-provide-project-require-files stx)
  (do-project-require-file-syntax stx #:provide? #t))

(define-syntax (require-project-require-files stx)
  (do-project-require-file-syntax stx))

(define-syntax (get-here-path stx)
  (datum->syntax stx 
                 '(begin 
                    ;; This macro might be used in an expression context, 
                    ;; so we use let, not define.
                    (let* ([ccr (current-contract-region)] ; trick for getting current module name
                           [here-path (cond
                                        ;; if contract-region is called from within submodule,
                                        ;; you get a list
                                        ;; in which case, just grab the path from the front
                                        [(list? ccr) (car ccr)]
                                        ;; file isn't yet saved in drracket
                                        ;; 'pollen-lang-module name is applied by reader
                                        [(equal? 'pollen-lang-module ccr) 'nowhere] 
                                        [else ccr])])
                      (path->string here-path)))))



