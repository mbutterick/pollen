#lang racket/base
(require racket/port racket/file racket/rerequire racket/contract racket/path)
;(require "world.rkt" )

;;todo: why is pollen/top operating in this file?

(module+ test (require rackunit))

(provide render render-batch)

;; for shared use by eval & system
(define nowhere-port (open-output-nowhere))


;; mod-dates is a hash that takes lists of paths as keys,
;; and lists of modification times as values.
;; Reason: a templated page is a combination of two source files.
;; Because templates have a one-to-many relationship with source files,
;; Need to track template mod-date for each source file.
;; Otherwise a changed template will get reloaded only once, 
;; and after that get reported as being up to date.
;; Possible: store hash on disk so mod records are preserved 
;; between development sessions (prob a worthless optimization)
(define mod-dates (make-hash))

(define/contract (make-mod-dates-key paths)
  ((listof path?) . -> . (listof path?))
  (define project-require-files (or (get-project-require-files) empty))
  (flatten (append paths project-require-files)))

;; convert a path to a modification date value
(define/contract (path->mod-date-value path)
  (path? . -> . (or/c exact-integer? #f))
  (and (file-exists? path) ; returns #f if a file doesn't exist
       (file-or-directory-modify-seconds path)))

(module+ test
  (check-false (path->mod-date-value (->path "foobarfoo.rkt")))
  (check-true (exact-integer? (path->mod-date-value (build-path (current-directory) (->path "render.rkt"))))))

;; put list of paths into mod-dates
;; need list as input (rather than individual path)
;; because hash key needs to be a list
;; so it's convenient to use a rest argument
;; therefore, use function by just listing out the paths
(define/contract (store-render-in-mod-dates . rest-paths)
  (() #:rest (listof path?) . ->* . void?)
  ;; project require files are appended to the mod-date key.
  ;; Why? So a change in a require file will trigger a render 
  ;; (which is the right thing to do, since pollen files are 
  ;; dependent on those requires)
  ;; It's convenient for development, because otherwise
  ;; you'd need to restart the server when you change a require
  ;; or explicitly use the force parameter.
  ;; This way, require files and pollen files have the same behavior.
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
(define/contract (reset-mod-dates)
  (-> void?)
  (set! mod-dates (make-hash)))

(module+ test 
  (reset-mod-dates)
  (store-render-in-mod-dates (build-path (current-directory) (->path "render.rkt")))
  (reset-mod-dates)
  (check-true (= (len mod-dates) 0)))

;; how to know whether a certain combination of paths needs a render
;; use rest argument here so calling pattern matches store-render
(define/contract (mod-date-expired? . rest-paths)
  (() #:rest (listof path?) . ->* . boolean?)
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


;; convenience function for external modules to use
(define/contract (render-batch . xs)
  (() #:rest (listof pathish?) . ->* . void?)
  ;; This will trigger rendering of all files.
  ;; Why not pass #:force #t through with render?
  ;; Because certain files will pass through multiple times (e.g., templates)
  ;; And with #:force, they would be rendered repeatedly.
  ;; Using reset-mod-dates is sort of like session control:
  ;; setting a state that persists through the whole operation.
  (reset-mod-dates) 
  (for-each render xs))

;; dispatches path to the right rendering function
;; use #:force to render regardless of cached state
(define/contract (render #:force [force #f] . xs)
  (() (#:force boolean?) #:rest (listof pathish?) . ->* . void?)
  (define (&render x) 
    (let ([path (->complete-path x)])              
      ;   (message "Dispatching render for" (->string (file-name-from-path path)))
      (cond
        ;; this will catch preprocessor files
        [(needs-preproc? path) (render-preproc-source path #:force force)]
        ;; this will catch pollen source files, 
        ;; and files without extension that correspond to p files
        [(needs-template? path) (render-with-template path #:force force)]
        ;; this will catch ptree files
        [(ptree-source? path) (let ([ptree (dynamic-require path 'main)])
                                (render-files-in-ptree ptree #:force force))]
        [(equal? FALLBACK_TEMPLATE (->string (file-name-from-path path)))
         (message "Render: using fallback template")]
        [(file-exists? path) (message "Serving static file" (->string (file-name-from-path path)))])))
  (for-each &render xs))

;; todo: write tests


(define/contract (rendering-message path)
  (any/c . -> . void?)
  ;; you can actually stuff whatever string you want into path —
  ;; if it's not really a path, file-name-from-path won't choke
  (message "Rendering" (->string (file-name-from-path path))))

(define/contract (rendered-message path)
  (any/c . -> . void?)
  (message "Rendered" (->string (file-name-from-path path))))

(define/contract (up-to-date-message path)
  (any/c . -> . void?)
  (message (->string (file-name-from-path path)) "is up to date, using cached copy"))


(define/contract (render-preproc-source x #:force [force #f])
  (((and/c pathish?
           (flat-named-contract 'file-exists
                                (λ(x) (file-exists? (->complete-path (->preproc-source-path x))))))) (#:force boolean?) . ->* . void?)
  
  ;; x might be either a preproc-source path or preproc-output path
  (define source-path (->complete-path (->preproc-source-path x)))
  (define-values (source-dir source-name _) (split-path source-path))
  (define output-path (->complete-path (->output-path x)))
  
  (define source-reloaded? (handle-source-rerequire source-path))
  
  ;; Four conditions under which we render preproc sources:
  (if (or
       ;; 1) explicitly forced render:
       force 
       ;; 2) output file doesn't exist (so it definitely won't appear in mod-dates)
       ;; also, this is convenient for development: 
       ;; you can trigger a render just by deleting the file
       (not (file-exists? output-path))
       ;; 3) file otherwise needs render (e.g., it changed)
       (mod-date-expired? source-path)
       ;; 4) source had to be reloaded (some other change)
       source-reloaded?)
      
      ;; how we render: import 'main from preproc source file, 
      ;; which is rendered during source parsing,
      ;; and write that to output path
      (begin
        (rendering-message (format "~a from ~a" 
                                   (file-name-from-path output-path)
                                   (file-name-from-path source-path)))
        (let ([main (time (render-through-eval source-dir `(dynamic-require ,source-path 'main)))])
          (display-to-file main output-path #:exists 'replace))
        (store-render-in-mod-dates source-path) ; don't store mod date until render has completed!
        (rendered-message output-path))
      
      ;; otherwise, skip file because there's no trigger for render
      (up-to-date-message output-path)))

;; todo: write tests


;; utility function for render-with-template
(define/contract (handle-source-rerequire source-path)
  ((and/c path? file-exists?) . -> . boolean?)
  
  ;; dynamic-rerequire watches files to see if they change.
  ;; if so, then it reloads them.
  ;; therefore, it's useful in a development environment
  ;; because it reloads as needed, but otherwise not.
  
  (define-values (source-dir source-name _) (split-path source-path))
  ;; need to require source file (to retrieve template name, which is in metas)
  ;; but use dynamic-rerequire now to force render for dynamic-require later,
  ;; otherwise the source file will cache
  ;; by default, rerequire reports reloads to error port.
  ;; set up a port to catch messages from dynamic-rerequire
  ;; and then examine this message to find out if anything was reloaded. 
  (define port-for-catching-file-info (open-output-string))
  
  ;; parameterize needed because source files have relative requires in project directory
  ;; parameterize is slow, IIRC
  (parameterize ([current-directory source-dir]
                 [current-error-port port-for-catching-file-info])
    (dynamic-rerequire source-path))
  
  ;; if the file needed to be reloaded, there will be a message in the port
  ;; this becomes the return value
  (->boolean (> (len (get-output-string port-for-catching-file-info)) 0)))

(define (complete-decoder-source-path x)
  (->complete-path (->decoder-source-path (->path x))))

;; apply template
(define/contract (render-with-template x [template-name #f] #:force [force #f]) 
  (((and/c pathish? 
           (flat-named-contract 'file-exists
                                (λ(x) (file-exists? (complete-decoder-source-path x))))))
   (path? #:force boolean?) . ->* . void?)
  
  ;; set up information about source
  (define source-path (complete-decoder-source-path x))
  ;; todo: this won't work with source files nested down one level
  (define-values (source-dir ignored also-ignored) (split-path source-path))
  
  ;; find out whether source had to be reloaded
  (define source-reloaded? (handle-source-rerequire source-path))
  
  ;; Then the rest: 
  ;; set the template, render the source file with template, and catch the output.
  ;; 1) Set the template. 
  (define template-path 
    (or 
     ;; Build the possible paths and use the first one  
     ;; that either exists, or has a preproc source that exists.
     (ormap (λ(p) (if (ormap file-exists? (list p (->preproc-source-path p))) p #f)) 
            (filter (λ(x) (->boolean x)) ;; if any of the possibilities below are invalid, they return #f 
                    (list
                     ;; path based on template-name
                     (and template-name (build-path source-dir template-name))
                     ;; path based on metas
                     (let ([source-metas (dynamic-require source-path 'metas)])
                       (and (TEMPLATE_META_KEY . in? . source-metas)
                            (build-path source-dir (get source-metas TEMPLATE_META_KEY))))
                     ;; path using default template name =
                     ;; "-main" + extension from output path (e.g. foo.xml.p -> -main.xml)
                     (build-path source-dir (add-ext DEFAULT_TEMPLATE_PREFIX (get-ext (->output-path source-path)))))))
     ;; if none of these work, make fallback template file
     (let ([ft-path (build-path source-dir FALLBACK_TEMPLATE)])
       (display-to-file fallback-template-data ft-path #:exists 'replace)
       ft-path)))
  
  
  ;; render template (it might have its own preprocessor file)
  (render template-path #:force force)
  
  ;; calculate new path for generated file
  (define output-path (->output-path source-path))
  
  ;; 2) Render the source file with template, if needed.
  ;; Render is expensive, so we avoid it when we can.
  ;; Four conditions where we render:
  (if (or force ; a) it's explicitly demanded
          (not (file-exists? output-path)) ; b) output file does not exist
          ;; c) mod-dates indicates render is needed
          (mod-date-expired? source-path template-path) 
          ;; d) dynamic-rerequire indicates the source had to be reloaded
          source-reloaded?)
      (begin
        (message "Rendering source" (->string (file-name-from-path source-path)) 
                 "with template" (->string (file-name-from-path template-path)))
        (let ([page-result (time (render-source-with-template source-path template-path))])
          (display-to-file page-result output-path #:exists 'replace)
          (store-render-in-mod-dates source-path template-path)
          (rendered-message output-path)))
      (up-to-date-message output-path))
  
  ;; delete fallback template if needed
  (let ([ft-path (build-path source-dir FALLBACK_TEMPLATE)])
    (when (file-exists? ft-path) (delete-file ft-path))))

;; cache some modules inside this namespace so they can be shared by namespace for eval
;; todo: macrofy this to avoid repeating names
(require web-server/templates  
         racket/list
         xml/path
         pollen/debug
         pollen/decode
         pollen/file-tools
         pollen/main-imports
         pollen/main-preproc-imports
         pollen/predicates
         pollen/ptree
         sugar
         pollen/template
         pollen/tools
         pollen/world)
(define original-ns (current-namespace))

(define/contract (render-through-eval base-dir eval-string)
  (directory-pathish? list? . -> . string?)
  (parameterize ([current-namespace (make-base-empty-namespace)]
                 [current-directory (->complete-path base-dir)]
                 [current-output-port nowhere-port])
    ;; attach already-imported modules 
    ;; this is a performance optimization: this way,
    ;; the eval namespace doesn't have to re-import these
    ;; because otherwise, most of its time is spent traversing imports.
    (map (λ(mod-name) (namespace-attach-module original-ns mod-name)) 
         '(racket/base 
           web-server/templates 
           xml/path
           racket/port 
           racket/file 
           racket/rerequire 
           racket/contract 
           racket/list
           pollen/debug
           pollen/decode
           pollen/file-tools
           pollen/main-imports
           pollen/main-preproc-imports
           pollen/predicates
           pollen/ptree
           sugar
           pollen/template
           pollen/tools
           pollen/world))
    (namespace-require 'racket/base) ; use namespace-require for FIRST require, then eval after
    (eval '(require (for-syntax racket/base)))
    (eval eval-string (current-namespace))))

(define/contract (render-source-with-template source-path template-path)
  (file-exists? file-exists? . -> . string?)
  
  ;; set up information about source and template paths
  ;; todo: how to write these without blanks?
  (define-values (source-dir source-name _) (split-path source-path))
  (define-values (___ template-name __) (split-path template-path))
  
  ;; Templates are part of the compile operation.
  ;; Therefore no way to arbitrarily invoke template at run-time.
  ;; This routine creates a new namespace and compiles the template within it.
  
  (define string-to-eval 
    `(begin 
      ;; for include-template (used below)
      (require web-server/templates)
      ;; for ptree navigation functions, and template commands
      ;; todo: main-helper is here for #%top and bound/c — should they go elsewhere?
      (require pollen/debug pollen/ptree pollen/template pollen/top)
      ;; import source into eval space. This sets up main & metas
      (require ,(->string source-name))
      (parameterize ([current-ptree (make-project-ptree ,PROJECT_ROOT)]
                     [current-url-context ,PROJECT_ROOT])
        (include-template #:command-char ,TEMPLATE_FIELD_DELIMITER ,(->string template-name)))))
  
  (render-through-eval source-dir string-to-eval))


;; render files listed in a ptree file
(define/contract (render-files-in-ptree ptree #:force [force #f])
  ((ptree?) (#:force boolean?) . ->* . void?)    
  ;; pass force parameter through 
  (for-each (λ(i) (render i #:force force)) 
            ;; use dynamic-require to avoid requiring ptree.rkt every time render.rkt is required
            ((dynamic-require "ptree.rkt" 'all-pages) ptree)))



;; todo: write test
