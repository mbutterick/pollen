#lang br
(require racket/path
         pollen/private/file-utils
         racket/file)

(define sources (for/list ([p (in-directory ".")]
                           #:when (path-has-extension? p #"pm"))
                          p))

#R sources

(define temp-template (build-path "/Users/MB/Desktop/sources/template.txt"))
(define output-paths (map simplify-path (map path->complete-path (map ->output-path sources))))
#R output-paths

(define render-results
(eval (with-syntax ([MODNAME (gensym)]
                    [TEMPLATE-PATH-STRING (path->string temp-template)])
        #`(begin
            (println 'MODNAME)
            (module MODNAME pollen/private/gang-helper
              #:sources #,@(map path->string sources)
              #:template TEMPLATE-PATH-STRING
              #:result-id result)
            (let ()
              (local-require 'MODNAME)
              result)))))

(for ([render-result (in-list render-results)]
      [output-path (in-list output-paths)])
     (display-to-file render-result
                       output-path
                       #:exists 'replace
                       #:mode (if (string? render-result) 'text 'binary)))