#lang racket/base
(require racket/file racket/rerequire racket/path racket/match)
(require sugar "file-tools.rkt" "cache.rkt" "world.rkt" "debug.rkt" "ptree.rkt" "project-requires.rkt")


;; when you want to generate everything fresh, 
;; but without having to #:force everything.
;; render functions will always go when no mod-date is found.
(define (reset-modification-dates)
  (set! modification-date-hash (make-hash)))

;; mod-dates is a hash that takes lists of paths as keys,
;; and lists of modification times as values.
(define modification-date-hash #f)
(reset-modification-dates)

;; using internal contracts to provide some extra safety (negligible performance hit)

(define/contract (make-mod-dates-key paths)
  ((listof (or/c #f complete-path?)) . -> . (listof (or/c #f complete-path?)))
  paths) ; for now, this does nothing; maybe later, it will do more


(define/contract (path->mod-date-value path)
  ((or/c #f complete-path?) . -> . (or/c #f integer?))
  (and path (file-exists? path) (file-or-directory-modify-seconds path)))


(define/contract (store-render-in-modification-dates . rest-paths)
  (() #:rest (listof (or/c #f complete-path?)) . ->* . void?)
  (define key (make-mod-dates-key rest-paths))
  (hash-set! modification-date-hash key (map path->mod-date-value key)))


(define/contract (modification-date-expired? . rest-paths)
  (() #:rest (listof (or/c #f complete-path?)) . ->* . boolean?)
  (define key (make-mod-dates-key rest-paths))
  (or (not (key . in? . modification-date-hash))  ; no stored mod date
      (not (equal? (map path->mod-date-value key) (get modification-date-hash key))))) ; data has changed


(define/contract+provide (render-batch . xs)
  (() #:rest (listof pathish?) . ->* . void?)
  ;; Why not pass #:force #t through with render?
  ;; Because certain files will pass through multiple times (e.g., templates)
  ;; And with #:force, they would be rendered repeatedly.
  ;; Using reset-modification-dates is sort of like session control.
  (reset-modification-dates) 
  (for-each render-to-file-if-needed xs))


(define/contract+provide (render-for-dev-server pathish #:force [force #f])
  ((pathish?) (#:force boolean?) . ->* . void?)
  (let ([so-path (->complete-path pathish)])  ; so-path = source or output path (could be either) 
    (cond
      [(ormap (λ(test) (and (test so-path) (render-to-file-if-needed so-path #:force force)))
              (list has/is-null-source? has/is-preproc-source? has/is-markup-source?))]
      [(ptree-source? so-path) (let ([ptree (cached-require so-path world:main-pollen-export)])
                                 (for-each (λ(pnode) (render-for-dev-server pnode #:force force)) (ptree->list ptree)))]))
  (void))


(define/contract (->source+output-paths source-or-output-path)
  (complete-path? . -> . (values complete-path? complete-path?))
  ;; file-proc returns two values, but ormap only wants one
  (define file-proc (ormap (λ(test file-proc) (and (test source-or-output-path) file-proc))
                           (list has/is-null-source? has/is-preproc-source? has/is-markup-source?)
                           (list ->null-source+output-paths ->preproc-source+output-paths ->markup-source+output-paths)))
  (file-proc source-or-output-path))


(define/contract (render-needed? source-path template-path output-path)
  (complete-path? (or/c #f complete-path?) complete-path? . -> . boolean?)
  (or (not (file-exists? output-path))
      (modification-date-expired? source-path template-path)
      (and (not (null-source? source-path)) (source-needs-rerequire? source-path))))


(define/contract+provide (render-to-file-if-needed source-or-output-path #:force [force #f])
  ((complete-path?) (#:force boolean?) . ->* . void?)  
  (define-values (source-path output-path) (->source+output-paths source-or-output-path))
  (define template-path (get-template-for source-path))
  (when (or force (render-needed? source-path template-path output-path))
    (render-to-file source-path template-path output-path)))


(define/contract+provide (render-to-file source-path [template-path #f] [maybe-output-path #f])
  ((complete-path?) ((or/c #f complete-path?) (or/c #f complete-path?)) . ->* . void?)
  (define output-path (or maybe-output-path (->output-path source-path)))
  (display-to-file (render source-path template-path) output-path #:exists 'replace))


(define/contract+provide (render source-path [template-path #f])
  ((complete-path?) ((or/c #f complete-path?)) . ->* . bytes?)
  (define render-proc 
    (cond
      [(ormap (λ(test render-proc) (and (test source-path) render-proc))
              (list has/is-null-source? has/is-preproc-source? has/is-markup-source?)
              (list render-null-source render-preproc-source render-markup-source))]
      [else (error (format "render: no rendering function found for ~a" source-path))]))
  
  (message (format "render: ~a" (file-name-from-path source-path)))
  (store-render-in-modification-dates source-path template-path) ; todo?: this may need to go after render
  (apply render-proc (cons source-path (if template-path (list template-path) null))))


(define/contract (render-null-source source-path)
  (complete-path? . -> . bytes?)
  ;; All this does is copy the source. Hence, "null".
  ;; todo: add test to avoid copying if unnecessary (good idea in case the file is large)
  (file->bytes source-path))


(define/contract (render-preproc-source source-path)
  (complete-path? . -> . bytes?)
  (match-define-values (source-dir _ _) (split-path source-path))
  (time (parameterize ([current-directory (->complete-path source-dir)])
          (render-through-eval `(begin (require pollen/cache)(cached-require ,source-path ',world:main-pollen-export))))))


(define/contract (render-markup-source source-path [maybe-template-path #f]) 
  ((complete-path?) ((or/c #f complete-path?)) . ->* . bytes?)
  (match-define-values (source-dir _ _) (split-path source-path)) ; todo: this won't work with source files nested down one level
  (define template-path (or maybe-template-path (get-template-for source-path)))
  (render-for-dev-server template-path) ; because template might have its own preprocessor source
  
  (define expr-to-eval 
    `(begin 
       (require (for-syntax racket/base))
       (require web-server/templates pollen/cache)
       (require pollen/lang/inner-lang-helper)
       (require-project-require-files) 
       (let ([doc (cached-require ,source-path ',world:main-pollen-export)]
             [metas (cached-require ,source-path ',world:meta-pollen-export)])
         (local-require pollen/debug pollen/ptree pollen/template pollen/top)
         (include-template #:command-char ,world:template-field-delimiter ,(->string (find-relative-path source-dir template-path))))))
  
  (time (parameterize ([current-directory source-dir])
          (render-through-eval expr-to-eval))))


(define/contract (templated-source? path)
  (complete-path? . -> . boolean?)
  (not (or (null-source? path) (preproc-source? path))))


(define/contract (get-template-for source-path)
  (complete-path? . -> . (or/c #f complete-path?))
  (match-define-values (source-dir _ _) (split-path source-path))
  (and (templated-source? source-path) ; doesn't make sense if it's not a templated source format
       (or ; Build the possible paths and use the first one that either exists, or has a preproc source that exists.
        (ormap (λ(p) (if (ormap file-exists? (list p (->preproc-source-path p))) p #f)) 
               (filter (λ(x) (->boolean x)) ; if any of the possibilities below are invalid, they return #f 
                       (list                     
                        (parameterize ([current-directory (world:current-project-root)])
                          (let ([source-metas (cached-require source-path 'metas)])
                            (and (world:template-meta-key . in? . source-metas)
                                 (build-path source-dir (get source-metas world:template-meta-key))))) ; path based on metas
                        (build-path source-dir 
                                    (add-ext (add-ext world:default-template-prefix (get-ext (->output-path source-path))) world:template-source-ext))))) ; path to default template
        (build-path (world:current-server-extras-path) world:fallback-template)))) ; fallback template


(define/contract (source-needs-rerequire? source-path)
  (complete-path? . -> . boolean?)
  (define-values (source-dir source-name _) (split-path source-path))
  ;; use dynamic-rerequire now to force render for cached-require later,
  ;; otherwise the source file will get cached by compiler
  (define port-for-catching-file-info (open-output-string))
  (parameterize ([current-directory source-dir]
                 [current-error-port port-for-catching-file-info])
    (dynamic-rerequire source-path))
  ;; if the file needed to be reloaded, there will be a message in the port
  (> (len (get-output-string port-for-catching-file-info)) 0))


;; cache some modules to speed up eval.
;; Do it in separate module so as not to pollute this one.
;; todo: macrofy these lists of modules
(module my-module-cache racket/base
  (require web-server/templates 
           xml
           racket/port 
           racket/file 
           racket/rerequire 
           racket/contract 
           racket/list
           racket/match
           pollen/debug
           pollen/decode
           pollen/file-tools
           pollen/main
           pollen/lang/inner-lang-helper
           pollen/predicates
           pollen/ptree
           pollen/cache
           sugar
           txexpr
           pollen/template
           pollen/tools
           pollen/world
           pollen/project-requires)
  (require-project-require-files)
  (define-namespace-anchor my-module-cache-ns-anchor)
  (provide my-module-cache-ns-anchor))


(require 'my-module-cache)
(define cache-ns (namespace-anchor->namespace my-module-cache-ns-anchor))


(define/contract (render-through-eval expr-to-eval)
  (list? . -> . bytes?)
  (parameterize ([current-namespace (make-base-namespace)]
                 [current-output-port (current-error-port)]
                 [current-ptree (make-project-ptree (world:current-project-root))]
                 [current-url-context (world:current-project-root)])
    (for-each (λ(mod-name) (namespace-attach-module cache-ns mod-name)) 
              `(web-server/templates 
                xml
                racket/port 
                racket/file 
                racket/rerequire 
                racket/contract 
                racket/list
                racket/match
                pollen/debug
                pollen/decode
                pollen/lang/inner-lang-helper
                pollen/predicates
                pollen/ptree
                pollen/cache
                sugar
                txexpr
                pollen/template
                pollen/tools
                pollen/world
                pollen/project-requires 
                ,@(get-project-require-files)))   
    (string->bytes/utf-8 (eval expr-to-eval (current-namespace)))))