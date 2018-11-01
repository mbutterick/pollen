#lang racket/base
(require (for-syntax racket/base
                     syntax/strip-context
                     pollen/private/project
                     pollen/setup)
         pollen/private/external/include-template
         pollen/cache
         pollen/pagetree
         pollen/core
         pollen/template
         pollen/top)

(provide (rename-out [mb #%module-begin])
         (except-out (all-from-out racket/base) #%module-begin))
(define-syntax (mb stx)
  (syntax-case stx ()
    [(_ SOURCE-PATH-STRING TEMPLATE-PATH-STRING)
     (let ([source-path (syntax->datum #'SOURCE-PATH-STRING)])
       (with-syntax ([DIRECTORY-REQUIRE-FILES
                      (replace-context #'here (require-directory-require-files source-path))]
                     [DOC-ID (setup:main-export source-path)]
                     [META-ID (setup:meta-export source-path)]
                     [CPR (current-project-root)]
                     [HERE-PATH-KEY (setup:here-path-key source-path)]
                     [COMMAND-CHAR (setup:command-char source-path)])
         #'(#%module-begin 
            DIRECTORY-REQUIRE-FILES
            (define result
              (parameterize ([current-pagetree (make-project-pagetree CPR)]
                             [current-metas (cached-metas SOURCE-PATH-STRING)])
                (define DOC-ID (cached-doc SOURCE-PATH-STRING))
                (define META-ID (current-metas))
                (define here (path->pagenode (or (select-from-metas 'HERE-PATH-KEY META-ID) 'unknown)))
                (if (bytes? DOC-ID) ; if main export is binary, just pass it through
                    DOC-ID
                    (include-template #:command-char COMMAND-CHAR (file TEMPLATE-PATH-STRING)))))
            (provide result))))]))