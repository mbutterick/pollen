#lang racket/base
(require (for-syntax racket/base))
(require racket/port racket/file racket/rerequire racket/path)
(require sugar)

(module+ test (require rackunit))

(define-syntax (define+provide+safe stx)
  (syntax-case stx ()
    [(_ (proc arg ... . rest-arg) contract body ...)
     #'(define+provide+safe proc contract
         (λ(arg ... . rest-arg) body ...))]
    [(_ name contract body ...)
     #'(begin
         (define name body ...)
         (provide name)
         (module+ safe 
           (provide (contract-out [name contract]))))]))


;; for shared use by eval & system
(define nowhere-port (open-output-nowhere))


;; mod-dates is a hash that takes lists of paths as keys,
;; and lists of modification times as values.
(define mod-dates (make-hash))


(define (make-mod-dates-key paths)
  ;; project require files are appended to the mod-date path key.
  ;; Why? So a change in a require file will trigger a render 
  (define project-require-files (or (get-project-require-files) empty))
  (flatten (append paths project-require-files)))


(define (path->mod-date-value path)
  (and (file-exists? path) ; returns #f if a file doesn't exist
       (file-or-directory-modify-seconds path)))


(module+ test
  (check-false (path->mod-date-value (->path "nonexistent-file-is-false.rkt"))))


(define (store-render-in-mod-dates . rest-paths)
  (define key (make-mod-dates-key rest-paths))
  (hash-set! mod-dates key (map path->mod-date-value key)))

(module+ test
  (reset-mod-dates)
  (store-render-in-mod-dates (build-path (current-directory) (->path "render.rkt")))
  (check-true (= (len mod-dates) 1))
  (reset-mod-dates))


;; when you want to generate everything fresh, 
;; but without having to #:force everything.
;; render functions will always go when no mod-date is found.
(define (reset-mod-dates)
  (set! mod-dates (make-hash)))

(module+ test 
  (reset-mod-dates)
  (store-render-in-mod-dates (build-path (current-directory) (->path "render.rkt")))
  (reset-mod-dates)
  (check-true (= (len mod-dates) 0)))


(define (mod-date-expired? . rest-paths)
  (define key (make-mod-dates-key rest-paths))
  (or (not (key . in? . mod-dates))  ; no stored mod date
      (not (equal? (map path->mod-date-value key) (get mod-dates key))))) ; data has changed

(module+ test 
  (reset-mod-dates)
  (let ([path (build-path (current-directory) (->path "render.rkt"))])
    (store-render-in-mod-dates path)
    (check-false (mod-date-expired? path))
    (reset-mod-dates)
    (check-true (mod-date-expired? path))))


(define+provide/contract (render-batch . xs)
  (() #:rest (listof pathish?) . ->* . void?)
  ;; This will trigger rendering of all files.
  ;; Why not pass #:force #t through with render?
  ;; Because certain files will pass through multiple times (e.g., templates)
  ;; And with #:force, they would be rendered repeatedly.
  ;; Using reset-mod-dates is sort of like session control:
  ;; setting a state that persists through the whole operation.
  (reset-mod-dates) 
  (for-each render xs))


(define+provide/contract (render #:force [force #f] . xs)
  (() (#:force boolean?) #:rest (listof pathish?) . ->* . void?)
  (define (&render x) 
    (let ([path (->complete-path x)])              
      (cond
        [(needs-null? path) (render-null-source path #:force force)]
        [(needs-preproc? path) (render-preproc-source-if-needed path #:force force)]
        [(needs-template? path) (render-with-template path #:force force)]
        [(ptree-source? path) (let ([ptree (cached-require path 'main)])
                                (render-files-in-ptree ptree #:force force))]
        [(equal? FALLBACK_TEMPLATE (->string (file-name-from-path path)))
         (message "Render: using fallback template")]
        [(file-exists? path) (message "Serving static file" (->string (file-name-from-path path)))])))
  (for-each &render xs))

;; todo: write tests


(define (rendering-message path)
  (message "Rendering" (->string (file-name-from-path path))))

(define (rendered-message path)
  (message "Rendered" (->string (file-name-from-path path))))

(define (up-to-date-message path)
  (message (->string (file-name-from-path path)) "is up to date, using cached copy"))

(define (render-null-source path #:force force)
  ;; this op is trivial & fast, so do it every time.
  (define source-path (->complete-path (->null-source-path path)))
  (define output-path (->complete-path (->output-path path)))
  (message (format "Copying ~a to ~a" 
                             (file-name-from-path source-path)
                             (file-name-from-path output-path)))
  (copy-file source-path output-path #t))

(define (render-preproc-source source-path output-path)
  ;; how we render: import 'main from preproc source file, 
  ;; which is rendered during source parsing, and write that to output path
  (define-values (source-dir source-name _) (split-path source-path))
  (rendering-message (format "~a from ~a" 
                             (file-name-from-path output-path)
                             (file-name-from-path source-path)))
  (let ([main (time (render-through-eval source-dir `(begin (require pollen/cache)(cached-require ,source-path 'main))))]) ;; todo: how to use world global here? Wants an identifier, not a value
    (display-to-file main output-path #:exists 'replace))
  (store-render-in-mod-dates source-path) ; don't store mod date until render has completed!
  (rendered-message output-path))

(define (render-preproc-source-if-needed input-path #:force [force-render #f])
  
  ;; input-path might be either a preproc-source path or preproc-output path
  ;; But the coercion functions will figure it out.
  (define source-path (->complete-path (->preproc-source-path input-path)))
  (define output-path (->complete-path (->output-path input-path)))
  
  (define render-needed? 
    (or
     force-render 
     (not (file-exists? output-path))
     (mod-date-expired? source-path)
     (let ([source-reloaded? (handle-source-rerequire source-path)])
       source-reloaded?)))
  
  (if render-needed?
      (render-preproc-source source-path output-path)      
      (up-to-date-message output-path)))

;; todo: write tests


(define (handle-source-rerequire source-path)
  (define-values (source-dir source-name _) (split-path source-path))
  ;; use dynamic-rerequire now to force render for cached-require later,
  ;; otherwise the source file will get cached by compiler
  (define port-for-catching-file-info (open-output-string))
  (parameterize ([current-directory source-dir]
                 [current-error-port port-for-catching-file-info])
    (dynamic-rerequire source-path))
  ;; if the file needed to be reloaded, there will be a message in the port
  (->boolean (> (len (get-output-string port-for-catching-file-info)) 0)))


(define (complete-decoder-source-path x)
  (->complete-path (->decoder-source-path (->path x))))


(define (render-with-template x [template-name #f] #:force [force-render #f]) 
  (define source-path (complete-decoder-source-path x))
  ;; todo: this won't work with source files nested down one level
  (define-values (source-dir ignored also-ignored) (split-path source-path))
  
  ;; Then the rest: 
  ;; 1) Set the template. 
  (define template-path 
    (or 
     ;; Build the possible paths and use the first one that either exists, or has a preproc source that exists.
     (ormap (λ(p) (if (ormap file-exists? (list p (->preproc-source-path p))) p #f)) 
            (filter (λ(x) (->boolean x)) ; if any of the possibilities below are invalid, they return #f 
                    (list                     
                     (and template-name (build-path source-dir template-name)) ; path based on template-name
                     (parameterize ([current-directory (CURRENT_PROJECT_ROOT)])
                       (let ([source-metas (cached-require source-path 'metas)])
                         (and (TEMPLATE_META_KEY . in? . source-metas)
                              (build-path source-dir (get source-metas TEMPLATE_META_KEY))))) ; path based on metas
                     (build-path source-dir 
                                 (add-ext (add-ext DEFAULT_TEMPLATE_PREFIX (get-ext (->output-path source-path))) TEMPLATE_EXT))))) ; path using default template
     (let ([ft-path (build-path source-dir FALLBACK_TEMPLATE)]) ; if none of these work, make fallback template file
       (copy-file (build-path (current-server-extras-path) FALLBACK_TEMPLATE) ft-path #t)
       ft-path)))
  
  (render template-path #:force force-render) ; bc template might have its own preprocessor source
  (define output-path (->output-path source-path))
  
  ;; 2) Render the source file with template, if needed.
  ;; Render is expensive, so we avoid it when we can. Four conditions where we render:
  (if (or force-render ; a) it's explicitly demanded
          (not (file-exists? output-path)) ; b) output file does not exist
          (mod-date-expired? source-path template-path) ; c) mod-dates indicates render is needed
          (let ([source-reloaded? (handle-source-rerequire source-path)]) ; d) dynamic-rerequire says refresh needed
            source-reloaded?))
      (begin
        (message "Rendering source" (->string (file-name-from-path source-path)) 
                 "with template" (->string (file-name-from-path template-path)))
        (let ([page-result (time (render-source-with-template source-path template-path))])
          (display-to-file page-result output-path #:exists 'replace)
          (store-render-in-mod-dates source-path template-path)
          (rendered-message output-path)))
      (up-to-date-message output-path))
  
  (let ([ft-path (build-path source-dir FALLBACK_TEMPLATE)]) ; delete fallback template if needed
    (when (file-exists? ft-path) (delete-file ft-path))))

;; cache some modules inside this namespace so they can be shared by namespace for eval
;; todo: macrofy this to avoid repeating names
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
         ;;         pollen/file-tools
         ;; not pollen/main, because it brings in pollen/top
         pollen/lang/inner-lang-helper
         pollen/predicates ;; exports file-tools
         pollen/ptree
         pollen/cache
         sugar
         txexpr
         pollen/template
         pollen/tools
         ;; not pollen/top, because we don't want it in the current ns
         pollen/world
         pollen/project-requires)
(define original-ns (current-namespace))

(define (render-through-eval base-dir eval-string)
  (parameterize ([current-namespace (make-base-namespace)]
                 [current-directory (->complete-path base-dir)]
                 [current-output-port (current-error-port)]
                 [current-ptree (make-project-ptree (CURRENT_PROJECT_ROOT))]
                 [current-url-context (CURRENT_PROJECT_ROOT)])
    (for-each (λ(mod-name) (namespace-attach-module original-ns mod-name)) 
              '(web-server/templates 
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
                pollen/project-requires))   
    (eval eval-string (current-namespace))))


(define (render-source-with-template source-path template-path)
  
  (match-define-values (source-dir source-name _) (split-path source-path))
  (match-define-values (_ template-name _) (split-path template-path))
  
  (set! source-name (->string source-name))
  (define string-to-eval 
    `(begin 
       (require (for-syntax racket/base))
       (require web-server/templates pollen/cache)
       ;; we could require the source-name directly,
       ;; and get its exports and also the project-requires transitively.
       ;; but this is slow.
       ;; So do it separately: require the project require files on their own,
       ;; then fetch the other exports out of the cache.
       (require pollen/lang/inner-lang-helper)
       (require-project-require-files) 
       (let ([main (cached-require ,source-name 'main)]
             [metas (cached-require ,source-name 'metas)])
         (local-require pollen/debug pollen/ptree pollen/template pollen/top)
         (include-template #:command-char ,TEMPLATE_FIELD_DELIMITER ,(->string template-name)))))
  
  (render-through-eval source-dir string-to-eval))

#|
(module+ main
  (parameterize ([current-cache (make-cache)]
                 [CURRENT_PROJECT_ROOT (string->path "/Users/mb/git/bpt")])
    (render-source-with-template
     (string->path "/Users/mb/git/bpt/test.html.pm")
     (string->path "/Users/mb/git/bpt/-test.html"))))
|#



(define (render-files-in-ptree ptree #:force [force #f])    
  (for-each (λ(i) (render i #:force force)) 
            ((cached-require "ptree.rkt" 'all-pages) ptree)))



;; todo: write test
