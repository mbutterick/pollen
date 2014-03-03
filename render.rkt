#lang racket/base
(require (for-syntax racket/base))
(require racket/port racket/file racket/rerequire racket/path racket/list racket/match)
(require sugar "file-tools.rkt" "cache.rkt" "world.rkt" "debug.rkt" "ptree.rkt" "project-requires.rkt")

;; for shared use by eval & system
(define nowhere-port (open-output-nowhere))


;; mod-dates is a hash that takes lists of paths as keys,
;; and lists of modification times as values.
(define mod-dates (make-hash))


(define (make-mod-dates-key paths)
  (flatten paths))


(define (path->mod-date-value path)
  (and (file-exists? path) ; returns #f if a file doesn't exist
       (file-or-directory-modify-seconds path)))


(define (store-render-in-mod-dates . rest-paths)
  (define key (make-mod-dates-key rest-paths))
  (hash-set! mod-dates key (map path->mod-date-value key)))


;; when you want to generate everything fresh, 
;; but without having to #:force everything.
;; render functions will always go when no mod-date is found.
(define (reset-mod-dates)
  (set! mod-dates (make-hash)))


(define (mod-date-expired? . rest-paths)
  (define key (make-mod-dates-key rest-paths))
  (or (not (key . in? . mod-dates))  ; no stored mod date
      (not (equal? (map path->mod-date-value key) (get mod-dates key))))) ; data has changed



(define+provide/contract (render-batch . xs)
  (() #:rest (listof pathish?) . ->* . void?)
  ;; This will trigger rendering of all files.
  ;; Why not pass #:force #t through with render?
  ;; Because certain files will pass through multiple times (e.g., templates)
  ;; And with #:force, they would be rendered repeatedly.
  ;; Using reset-mod-dates is sort of like session control:
  ;; setting a state that persists through the whole operation.
  (reset-mod-dates) 
  (for-each render-for-dev-server xs))


(define+provide/contract (render-for-dev-server pathish #:force [force #f])
  ((pathish?) (#:force boolean?) . ->* . void?)
  (let ([path (->complete-path pathish)])              
    (cond
      [(ormap (λ(test) (and (test path) (render-to-file path #:force force)))
              (list has/is-null-source? has/is-preproc-source? has/is-markup-source?))]
      [(ptree-source? path) (let ([ptree (cached-require path world:main-pollen-export)])
                              (for-each (λ(pnode) (render-for-dev-server pnode #:force force)) (ptree->list ptree)))]))
  (void))


(define (->source+output-paths source-or-output-path)
  ;; file-proc returns two values
  (define file-proc (ormap (λ(test file-proc) (and (test source-or-output-path) file-proc))
                           (list has/is-null-source? has/is-preproc-source? has/is-markup-source?)
                           (list ->null-source+output-paths ->preproc-source+output-paths ->markup-source+output-paths)))
  (file-proc source-or-output-path))


(define (render-to-file source-or-output-path [maybe-output-path #f] #:force [force #f])
  (define-values (source-path output-path) (->source+output-paths source-or-output-path))
  (when maybe-output-path (set! output-path maybe-output-path))
  (display-to-file (render source-path output-path #:force force) output-path #:exists 'replace))


(define (render source-path [output-path #f] #:force [force #f])
  (define render-proc 
    (cond
      [(ormap (λ(test render-proc) (and (test source-path) render-proc))
              (list has/is-null-source? has/is-preproc-source? has/is-markup-source?)
              (list render-null-source render-preproc-source render-markup-source))]
      [else (error (format "render: no rendering function found for ~a" source-path))]))
  
  (define render-needed? 
    (or force 
        (not (file-exists? (or output-path (->output-path source-path))))
        (mod-date-expired? source-path)
        (source-needs-rerequire? source-path)))
  
  (if render-needed?
      (let ([result (render-proc source-path)])
        (message (format "Rendered ~a" (file-name-from-path source-path)))
        (store-render-in-mod-dates source-path)
        result)
      (message (->string (file-name-from-path source-path)) "is up to date, using existing copy")))


(define/contract (render-null-source source-path)
  (pathish? . -> . bytes?)
  ;; All this does is copy the source. Hence, "null".
  ;; todo: add test to avoid copying if unnecessary
  ;; (good idea in case the file is large)
  (let ([source-path (->path source-path)])
    (file->bytes source-path)))


(define (render-preproc-source source-path)
  ;; how we render: import world:main-pollen-export from preproc source file, 
  ;; which is rendered during source parsing, and write that to output path
  (match-define-values (source-dir _ _) (split-path source-path))
  (time (parameterize ([current-directory (->complete-path source-dir)])
          (render-through-eval `(begin (require pollen/cache)(cached-require ,source-path ',world:main-pollen-export))))))


(define (render-markup-source source-path) 
  
  ;; todo: this won't work with source files nested down one level
  (match-define-values (source-dir _ _) (split-path source-path))
  
  ;; Then the rest: 
  ;; 1) Set the template. 
  (define template-path (get-template-for source-path))
  (render-for-dev-server template-path) ; because template might have its own preprocessor source
  
  ;; TODO: need to check  (mod-date-expired? source-path template-path))
  
  ;; 2) Render the source file with template, if needed
  (define string-to-eval 
    `(begin 
       (require (for-syntax racket/base))
       (require web-server/templates pollen/cache)
       (require pollen/lang/inner-lang-helper)
       (require-project-require-files) 
       (let ([doc (cached-require ,source-path ',world:main-pollen-export)]
             [metas (cached-require ,source-path ',world:meta-pollen-export)])
         (local-require pollen/debug pollen/ptree pollen/template pollen/top)
         (include-template #:command-char ,world:template-field-delimiter ,(->string template-path)))))
  
  (define result (time (parameterize ([current-directory (->complete-path source-dir)])
                         (render-through-eval string-to-eval))))
  
  (let ([ft-path (build-path source-dir world:fallback-template)]) ; delete fallback template if needed
    (when (file-exists? ft-path) (delete-file ft-path)))
  
  result)


(define (get-template-for source-path)
  (match-define-values (source-dir _ _) (split-path source-path))
  ;; Build the possible paths and use the first one that either exists, or has a preproc source that exists.
  (or 
   (ormap (λ(p) (if (ormap file-exists? (list p (->preproc-source-path p))) p #f)) 
          (filter (λ(x) (->boolean x)) ; if any of the possibilities below are invalid, they return #f 
                  (list                     
                   (parameterize ([current-directory (world:current-project-root)])
                     (let ([source-metas (cached-require source-path 'metas)])
                       (and (world:template-meta-key . in? . source-metas)
                            (build-path source-dir (get source-metas world:template-meta-key))))) ; path based on metas
                   (build-path source-dir 
                               (add-ext (add-ext world:default-template-prefix (get-ext (->output-path source-path))) world:template-source-ext))))) ; path using default template
   (let ([ft-path (build-path source-dir world:fallback-template)]) ; if none of these work, make fallback template file
     (copy-file (build-path (world:current-server-extras-path) world:fallback-template) ft-path #t)
     ft-path)))


(define (source-needs-rerequire? source-path)
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


(define (render-through-eval eval-string)
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
    (eval eval-string (current-namespace))))