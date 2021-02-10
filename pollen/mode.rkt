#lang racket/base

#|
Implements the pollen/mode metalanguage. Certain values are hardcoded inside the Racket source, so we have to copy & paste, a little.

Note that pollen/mode uses a hardcoded #\◊, as the command char, NOT (setup:command-char),
because importing `pollen/setup` will create a loading loop
if pollen/mode were used in "pollen.rkt" (which is a likely place to use it)
Intractable problem; unavoidable limitation.
|#


;; because the reader "boots" from `pollen/mode`,
;; Racket looks for the `language-info` submodule in `pollen/mode`
;; so we just re-export the default.
(module language-info racket/base
  (require at-exp/lang/language-info)
  (provide (all-from-out at-exp/lang/language-info)))


;; adapted from
;; https://github.com/racket/racket/blob/master/pkgs/at-exp-lib/at-exp/lang/reader.rkt

(module* reader racket/base
  (require syntax/module-reader
           (only-in scribble/reader make-at-readtable))
  
  (provide (rename-out [at-read read]
                       [at-read-syntax read-syntax]
                       [at-get-info get-info]))
  
  (define (wrap-reader p)
    (λ args
      (parameterize ([current-readtable (make-at-readtable #:datum-readtable 'dynamic
                                                           #:command-readtable 'dynamic
                                                           #:command-char #\◊)])
        (apply p args))))
  
  (define-values (at-read at-read-syntax at-get-info)
    (make-meta-reader
     'pollen/mode
     "language path"
     (λ (bstr)
       (let* ([str (bytes->string/latin-1 bstr)]
              [sym (string->symbol str)])
         (and (module-path? sym)
              (vector
               ;; try submod first:
               `(submod ,sym reader)
               ;; fall back to /lang/reader:
               (string->symbol (string-append str "/lang/reader"))))))
     wrap-reader
     (λ (orig-read-syntax)
       (define read-syntax (wrap-reader orig-read-syntax))
       (λ args
         (define stx (apply read-syntax args))
         (define old-prop (syntax-property stx 'module-language))
         (define new-prop `#(at-exp/lang/language-info get-language-info ,old-prop))
         (syntax-property stx 'module-language new-prop)))
     (λ (proc)
       (λ (key defval)
         (define (fallback) (if proc (proc key defval) defval))
         (define (try-dynamic-require mod export)
           (or (with-handlers ([exn:fail? (λ (x) #f)])
                 (dynamic-require mod export))
               (fallback)))
         (case key
           [(color-lexer)
            (define lexer-maker (dynamic-require 'syntax-color/scribble-lexer 'make-scribble-lexer (λ () #f)))
            (if lexer-maker
                (lexer-maker #:command-char #\◊)
                (fallback))]
           [(drracket:indentation)
            (dynamic-require 'pollen/private/external/mode-indentation 'determine-spaces)]
           [else (fallback)]))))))
