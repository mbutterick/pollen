#lang racket/base
(require (for-syntax racket/base))

(provide (all-defined-out))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copied out from racket/contract/private/base to avoid import
; Importing racket/contract/region is slow
; Used to be called current-contract-region
;
(require racket/stxparam syntax/location)

(define-syntax-parameter current-file-path
  (λ (stx)
    (if (eq? (syntax-local-context) 'expression)
        (let* ([ctxt (syntax-local-lift-context)]
               [id (hash-ref (make-hasheq) ctxt #f)])
          (with-syntax ([id (or id
                                (let ([id (syntax-local-lift-expression 
                                           (syntax/loc stx (quote-module-name)))])
                                  (hash-set! (make-hasheq) ctxt (syntax-local-introduce id))
                                  id))])
            #'id))
        (quasisyntax/loc stx (#%expression #,stx)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copied out from racket/path to avoid import
;
(define (pollen-find-relative-path directory filename #:more-than-root? [more-than-root? #f])
  
  (define (do-explode-path who orig-path)
    (define l (explode-path orig-path))
    (for ([p (in-list l)])
      (when (not (path-for-some-system? p))
        (raise-argument-error who 
                              "(and/c path-for-some-system? simple-form?)"
                              orig-path)))
    l)
  
  (let ([dir (do-explode-path 'find-relative-path directory)]
        [file (do-explode-path 'find-relative-path filename)])
    (if (and (equal? (car dir) (car file))
             (or (not more-than-root?)
                 (not (eq? 'unix (path-convention-type directory)))
                 (null? (cdr dir))
                 (null? (cdr file))
                 (equal? (cadr dir) (cadr file))))
        (let loop ([dir (cdr dir)]
                   [file (cdr file)])
          (cond [(null? dir) (if (null? file) filename (apply build-path file))]
                [(null? file) (apply build-path/convention-type 
                                     (path-convention-type filename)
                                     (map (lambda (x) 'up) dir))]
                [(equal? (car dir) (car file))
                 (loop (cdr dir) (cdr file))]
                [else
                 (apply build-path (append (map (lambda (x) 'up) dir) file))]))
        filename)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(define-syntax (get-here-path stx)
  #'(begin 
      (let* ([cfp (current-file-path)]
             [here-path (cond
                          ;; file isn't yet saved in drracket
                          ;; 'pollen-lang-module name is applied by reader
                          [(or (equal? 'pollen-lang-module cfp)
                               (and (list? cfp) (equal? (car cfp) 'pollen-lang-module)))
                           "unsaved-file"]
                          ;; if current-file-path is called from within submodule, you get a list
                          ;; in which case, just grab the path from the front
                          [(list? cfp) (path->string (car cfp))]
                          [else (path->string cfp)])])
        here-path)))


