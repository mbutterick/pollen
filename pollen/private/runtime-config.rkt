#lang racket/base
(provide configure)

(module show racket/base
  (require pollen/setup)  
  (provide show show-enabled)
  
  (define show-enabled (make-parameter #f))
  
  (define (show doc parser-mode here-path)
    ;; we only want the top doc to print in the runtime environment
    ;; otherwise if a Pollen source imports others, they will all print their docs in sequence.
    ;; so only print if the current here-path is the top path, which is stored in the `show-enabled` parameter.
    (when (and (show-enabled) (equal? here-path (show-enabled)))
      (if (or (eq? parser-mode default-mode-preproc)
              (eq? parser-mode default-mode-template))
          (display doc)
          ;; OK to use dynamic-require because runtime-config itself is dynamic-required
          (print (with-handlers ([exn:fail? (λ(exn) ((error '|pollen markup error| ((dynamic-require 'racket/string 'string-join) (cdr ((dynamic-require 'racket/string 'string-split) (exn-message exn) ": ")) ": "))))])
                   ((dynamic-require 'txexpr 'validate-txexpr) doc)))))))

(require 'show)

(define (configure top-here-path)
  (show-enabled top-here-path))

