#lang racket/base
(require (for-syntax racket/base racket/syntax "../setup.rkt" "split-metas.rkt")
         "to-string.rkt" "../pagetree.rkt" "splice.rkt" "../setup.rkt")
(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [dialect-module-begin #%module-begin]))

(define (stringify xs) (apply string-append (map to-string xs)))

(define (make-parse-proc parser-mode root-proc)
  (cond
    [(eq? parser-mode default-mode-pagetree) decode-pagetree]
    [(eq? parser-mode default-mode-markup) (λ (xs) (apply root-proc (remove-voids xs)))] 
    [(eq? parser-mode default-mode-markdown)
     (λ (xs) (let* ([xs (stringify xs)]
                    [xs ((dynamic-require 'markdown 'parse-markdown) xs)]
                    [xs (map strip-empty-attrs xs)])
               (apply root-proc xs)))]
    [else stringify])) ; preprocessor mode

(define (strip-leading-newlines doc)
  ;; drop leading newlines, as they're often the result of `defines` and `requires`
  (or (memf (λ (ln) (not (equal? ln (setup:newline)))) doc) null))

(define-for-syntax (make-pmb-macro parser-mode-from-expander)
  (λ (stx)
    (syntax-case stx ()
      [(_ . EXPRS)
       (let-values ([(meta-hash exprs-without-metas) (split-metas (syntax->datum #'EXPRS) (setup:define-meta-name))])
         (with-syntax (;; 'parser-mode-from-reader will be #f for an inline submodule
                       [PARSER-MODE-FROM-READER (syntax-property stx 'parser-mode-from-reader)]
                       [PARSER-MODE-FROM-EXPANDER parser-mode-from-expander]
                       [META-HASH meta-hash]
                       [EXPRS-WITHOUT-METAS exprs-without-metas]
                       [METAS-ID (setup:meta-export)]
                       [META-MOD-ID (setup:meta-export)]
                       [ROOT-ID (setup:main-root-node)]
                       [DOC-ID (setup:main-export)]
                       ;; prevents conflicts with other imported Pollen sources
                       [DOC-RAW (datum->syntax #'here (syntax->datum (generate-temporary 'pollen-)))])
           #'(#%module-begin
              (require pollen/top) ; could be at top of this module, but better to contain it
              
              (module META-MOD-ID racket/base
                (provide METAS-ID)
                (define METAS-ID META-HASH))
              (require 'META-MOD-ID)
                
              (module inner pollen/private/doclang-raw
                DOC-RAW ; positional arg for doclang-raw that sets name of export
                (require pollen/top pollen/core pollen/setup (submod ".." META-MOD-ID))
                (provide (all-defined-out))
                . EXPRS-WITHOUT-METAS)
              (require 'inner)
                
              (define DOC-ID
                ;; parser-mode must be resolved at runtime, not compile time
                (let* ([parser-mode (or 'PARSER-MODE-FROM-READER PARSER-MODE-FROM-EXPANDER)]
                       [proc (make-parse-proc parser-mode ROOT-ID)] 
                       [doc-elements (strip-leading-newlines DOC-RAW)]
                       [doc-elements-spliced (splice doc-elements (setup:splicing-tag))])
                  (proc doc-elements-spliced)))
                
              (provide DOC-ID METAS-ID (except-out (all-from-out 'inner) DOC-RAW)))))])))


(define-syntax-rule (dialect-module-begin PARSER-MODE-FROM-EXPANDER . READER-SUBMOD-AND-OTHER-EXPRS)
  (#%module-begin
   (require racket/base)
   (provide (except-out (all-from-out racket/base) #%module-begin)
            (rename-out [pollen-module-begin #%module-begin]))
   (define-syntax pollen-module-begin (make-pmb-macro 'PARSER-MODE-FROM-EXPANDER))
   . READER-SUBMOD-AND-OTHER-EXPRS))