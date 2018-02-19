#lang racket/base

(require (for-syntax racket/base
                     syntax/kerncase))

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [*module-begin #%module-begin]))

;; Module wrapper ----------------------------------------

(define-syntax (*module-begin stx)
  (syntax-case stx ()
    [(_ id post-process . body)
     (with-syntax ([exprs #'()])
       #'(#%module-begin
          (doc-begin id post-process exprs . body)))]))

(define-syntax (doc-begin stx)
  (syntax-case stx ()
    [(_ m-id post-process (expr ...))
     ;; unlike regular doclang, don't `provide` m-id (we'll do that in main-base wrapper)
     #`(define m-id (post-process (list . #,(reverse (syntax->list #'(expr ...))))))]
    [(_ m-id post-process exprs . body)
     ;; `body' probably starts with lots of string constants; it's
     ;; slow to trampoline on every string, so do them in a batch
     ;; here:
     (let loop ([body #'body]
                [accum null])
       (syntax-case body ()
         [(s . rest)
          (string? (syntax-e #'s))
          (loop #'rest (cons #'s accum))]
         [()
          (with-syntax ([(accum ...) accum])
            #`(doc-begin m-id post-process (accum ... . exprs)))]
         [(body1 . body)
          (with-syntax ([exprs (append accum #'exprs)])
            (let ([expanded (local-expand
                             #'body1 'module
                             (append (kernel-form-identifier-list)
                                     (syntax->list #'(provide
                                                      require
                                                      #%provide
                                                      #%require))))])
              (syntax-case expanded (begin)
                [(begin body1 ...)
                 #`(doc-begin m-id post-process exprs body1 ... . body)]
                [(id . rest)
                 (and (identifier? #'id)
                      (ormap (lambda (kw) (free-identifier=? #'id kw))
                             (syntax->list #'(require
                                               provide
                                               define-values
                                               define-syntaxes
                                               begin-for-syntax
                                               module
                                               module*
                                               #%require
                                               #%provide))))
                 #`(begin #,expanded (doc-begin m-id post-process exprs . body))]
                [_else
                 #`(doc-begin m-id post-process 
                              ((pre-part #,expanded body1) . exprs) 
                              . body)])))]))]))

(define-syntax (pre-part stx)
  (syntax-case stx ()
    [(_ s e)
     (if (string? (syntax-e #'s))
         #'s
         (with-syntax ([src (syntax-source #'e)]
                       [line (syntax-line #'e)]
                       [col (syntax-column #'e)]
                       [pos (syntax-position #'e)]
                       [span (syntax-column #'e)])
           #'(check-pre-part e (vector 'src 'line 'col 'pos 'span))))]))

(define (check-pre-part v s)
  v)