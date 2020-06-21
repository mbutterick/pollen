#lang racket/base
(require (for-syntax racket/base
                     syntax/strip-context
                     "project.rkt"
                     "../setup.rkt")
         racket/stxparam
         racket/splicing
         "external/include-template.rkt"
         "../cache.rkt"
         "../pagetree.rkt"
         "../core.rkt"
         "../setup.rkt"
         "../template.rkt"
         "../top.rkt")

(provide (rename-out [mb #%module-begin])
         (except-out (all-from-out racket/base) #%module-begin))

(define-syntax-parameter doc (λ (stx) (error 'doc-not-parameterized)))
(define-syntax-parameter metas (λ (stx) (error 'metas-not-parameterized)))
(define-syntax-parameter result (λ (stx) (error 'result-not-parameterized)))

(define-syntax (mb stx)
  (syntax-case stx ()
    ;; markup / markdown branch
    [(_ #:source SOURCE-PATH-STRING
        #:template TEMPLATE-PATH-STRING
        #:result-id RESULT-ID)
     (let ([source-path (syntax->datum #'SOURCE-PATH-STRING)])
       (with-syntax ([DIRECTORY-REQUIRE-FILES
                      (replace-context #'here (require-directory-require-files source-path))]
                     [DOC-ID pollen-main-export]
                     [METAS-ID pollen-meta-export]
                     [COMMAND-CHAR (setup:command-char source-path)])
         #'(#%module-begin 
            DIRECTORY-REQUIRE-FILES
            (splicing-syntax-parameterize
                ([doc (make-rename-transformer #'DOC-ID)]
                 [metas (make-rename-transformer #'METAS-ID)]
                 [result (make-rename-transformer #'RESULT-ID)])
              (define result
                (parameterize ([current-pagetree (make-project-pagetree (current-project-root))]
                               [current-metas (cached-metas SOURCE-PATH-STRING)])
                  (define doc (cached-doc SOURCE-PATH-STRING))
                  (define metas (current-metas))
                  (define here (path->pagenode
                                (or (select-from-metas pollen-here-path-key metas) 'unknown)))
                  (if (bytes? doc) ; if main export is binary, just pass it through
                      doc
                      ;; allows `require` in a template
                      (splicing-let-syntax ([require (make-rename-transformer #'local-require)])
                        (include-template #:command-char COMMAND-CHAR (file TEMPLATE-PATH-STRING))))))
              (provide result)))))]))