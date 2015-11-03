#lang racket/base

(module runtime-config racket/base
  (provide configure)
  
  (require (only-in "mode-helper.rkt" make-at-readtable))
  
  (define (configure data)
    (define old-read (current-read-interaction))
    (define (new-read src in)
      (parameterize ([current-readtable (make-at-readtable #:readtable (current-readtable))])
        (old-read src in)))
    (current-read-interaction new-read)))

(module language-info racket/base
  (provide get-language-info)
  
  (require racket/match)
  
  (define (get-language-info data)
    (define other-get-info
      (match data
        [(vector mod sym data2)
         ((dynamic-require mod sym) data2)]
        [_ (λ(key default) default)]))
    (λ(key default)
      (case key
        [(configure-runtime)
         (define config-vec '#[(submod pollen/mode runtime-config) configure #f])
         (define other-config (other-get-info key default))
         (cond [(list? other-config) (cons config-vec other-config)]
               [else (list config-vec)])]
        [else (other-get-info key default)]))))

(module reader racket/base
  (require syntax/module-reader pollen/world
           (only-in "mode-helper.rkt" make-at-readtable))
  
  (provide (rename-out [at-read read]
                       [at-read-syntax read-syntax]
                       [at-get-info get-info]))
  
  (define (wrap-reader p)
    (λ args
      (parameterize ([current-readtable (make-at-readtable #:datum-readtable 'dynamic
                                                           #:command-readtable 'dynamic
                                                           #:command-char (world:current-command-char))])
        (apply p args))))
  
  (define-values (at-read at-read-syntax at-get-info)
    (make-meta-reader
     'pollen/mode
     "language path"
     (λ(bstr)
       (let* ([str (bytes->string/latin-1 bstr)]
              [sym (string->symbol str)])
         (and (module-path? sym)
              (vector
               ;; try submod first:
               `(submod ,sym reader)
               ;; fall back to /lang/reader:
               (string->symbol (string-append str "/lang/reader"))))))
     wrap-reader
     (λ(orig-read-syntax)
       (define read-syntax (wrap-reader orig-read-syntax))
       (λ args
         (define stx (apply read-syntax args))
         (define old-prop (syntax-property stx 'module-language))
         (define new-prop `#((submod pollen/mode language-info) get-language-info ,old-prop))
         (syntax-property stx 'module-language new-prop)))
     (λ(proc)
       (λ(key defval)
         (define (fallback) (if proc (proc key defval) defval))
         (define (try-dynamic-require mod export)
           (or (with-handlers ([exn:fail? (λ(x) #f)])
                 (dynamic-require mod export))
               (fallback)))
         (case key
           [(color-lexer)
            (try-dynamic-require 'syntax-color/scribble-lexer 'scribble-lexer)]
           [(definitions-text-surrogate)
            'scribble/private/indentation]
           [(drracket:indentation)
            (dynamic-require 'scribble/private/indentation 'determine-spaces)]
           [else (fallback)]))))))