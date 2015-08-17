#lang racket/base
(provide configure)

(module show racket/base
  (require pollen/world)  
  (provide show show-enabled)
  
  (define show-enabled (make-parameter #f))
  
  (define (show doc parser-mode)
    (when (show-enabled)
      (if (or (equal? parser-mode world:mode-preproc)
              (equal? parser-mode world:mode-template))
          (display doc)
          (print (with-handlers ([exn:fail? (λ(exn) ((error '|pollen markup error| ((dynamic-require 'racket/string 'string-join) (cdr ((dynamic-require 'racket/string 'string-split) (exn-message exn) ": ")) ": "))))])
                   ((dynamic-require 'txexpr 'validate-txexpr) doc)))))))

(require 'show)

(define (configure data)
  (show-enabled #t))

