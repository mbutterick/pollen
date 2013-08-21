#lang racket/base
(require racket/list racket/path racket/port racket/system 
         racket/file racket/rerequire racket/contract)
(require "world.rkt" "tools.rkt" "pmap.rkt" "readability.rkt")

(module+ test (require rackunit))

(provide regenerate force-regenerate)

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


;; convert a path to a modification date value
(define/contract (path->mod-date-value path)
  (path? . -> . (or/c exact-integer? #f))
  (and (file-exists? path) ; returns #f if a file doesn't exist
       (file-or-directory-modify-seconds path)))

(module+ test
  (check-false (path->mod-date-value (->path "foobarfoo.rkt")))
  (check-true (exact-integer? (path->mod-date-value (build-path (current-directory) (->path "regenerate.rkt"))))))

;; put list of paths into mod-dates
;; want to take list as input (rather than individual path)
;; because hash key needs to be a list
(define/contract (store-refresh-in-mod-dates path-or-paths)
  ((or/c path? (listof path?)) . -> . void?)
  (define paths (->list path-or-paths))
  (hash-set! mod-dates paths (map path->mod-date-value paths)))

(module+ test
  (reset-mod-dates)
  (store-refresh-in-mod-dates (list (build-path (current-directory) (->path "regenerate.rkt"))))
  (check-true (= (len mod-dates) 1))
  (reset-mod-dates))

;; when you want to generate everything fresh, 
;; but without having to #:force everything.
;; Regenerate functions will always go when no mod-date is found.
(define/contract (reset-mod-dates)
  (-> void?)
  (set! mod-dates (make-hash)))

(module+ test 
  (reset-mod-dates)
  (store-refresh-in-mod-dates (list (build-path (current-directory) (->path "regenerate.rkt"))))
  (reset-mod-dates)
  (check-true (= (len mod-dates) 0)))

;; how to know whether a certain combination of paths needs a refresh
(define/contract (source-needs-refresh? path-or-paths)
  ((or/c path? (listof path?)) . -> . boolean?)
  (let ([paths (->list path-or-paths)])
    (or (not (paths . in? . mod-dates))  ; no stored mod date
        (not (equal? (map path->mod-date-value paths) (get mod-dates paths)))))) ; data has changed

(module+ test 
  (reset-mod-dates)
  (let ([path (build-path (current-directory) (->path "regenerate.rkt"))])
    (store-refresh-in-mod-dates (list path))
    (check-false (source-needs-refresh? (list path)))
    (reset-mod-dates)
    (check-true (source-needs-refresh? (list path)))))


;; convenience function for external modules to use
(define/contract (force-regenerate x)
  (pathish? . -> . void?)
  (regenerate x #:force #t))

;; dispatches path to the right regeneration function
;; use #:force to refresh regardless of cached state
(define/contract (regenerate x #:force [force #f])
  ((pathish?) (#:force boolean?) . ->* . void?)
  (let ([path (->complete-path (->path x))])
    (cond
      ;; this will catch pp (preprocessor) files
      [(needs-preproc? path) (regenerate-with-preproc path #:force force)]
      ;; this will catch p files, 
      ;; and files without extension that correspond to p files
      [(needs-template? path) (regenerate-with-template path #:force force)]
      ;; this will catch pmap (pollen map) files
      [(pmap-source? path) (let ([pmap (dynamic-require path 'main)])
                             (regenerate-with-pmap pmap #:force force))]
      [else (error "Regenerate: no handler for" (->string (file-name-from-path path)))])))

;; todo: write tests


;; todo: write contract & tests
(define (regenerating-message path)
  ;; you can actually stuff whatever string you want into path —
  ;; if it's not really a path, file-name-from-path won't choke
  (message "Regenerating:" (->string (file-name-from-path path))))

;; todo: write contract & tests
(define (regenerated-message path)
  (message "Regenerated:" (->string (file-name-from-path path))))


(define/contract (regenerate-with-preproc x #:force [force #f])
  ((pathish?) (#:force boolean?) . ->* . void?)
  (define path (->path x))
  ;; path might be either a preproc-source path or preproc-output path
  ;; figure out which, then compute the other
  (define-values (source-path output-path) (if (preproc-source? path) 
                                               (values path (make-preproc-output-path path))
                                               (values (make-preproc-source-path path) path)))  
  
  ;; Computing the source-path doesn't validate whether it exists.
  ;; Which is important, of course.
  (if (file-exists? source-path) 
      ;; Three conditions under which we refresh:
      (if (or
           ;; 1) explicitly forced refresh
           force 
           ;; 2) output file doesn't exist (so it definitely won't appear in mod-dates)
           ;; also, this is convenient for development: 
           ;; you can trigger a refresh just by deleting the file
           (not (file-exists? output-path)) 
           ;; 3) file otherwise needs refresh (e.g., it changed)
           (source-needs-refresh? source-path)) 
          ;; use single quotes to escape spaces in pathnames
          (let ([command (format "~a '~a' > '~a'" RACKET_PATH source-path output-path)])
            (regenerating-message (format "~a from ~a" 
                                          (file-name-from-path output-path)
                                          (file-name-from-path source-path)))
            (store-refresh-in-mod-dates source-path)
            ;; discard output using open-output-nowhere
            (parameterize ([current-output-port (open-output-nowhere)])
              (system command))
            (regenerated-message output-path))
          ;; otherwise, skip file because there's no trigger for refresh
          (message "File is up to date:" (->string (file-name-from-path output-path))))
      ;; source-path doesn't exist
      (message "Preprocessor file not found:" (->string (file-name-from-path source-path)))))

;; todo: write tests



;;;;;;;;;;;;;;;
;; todo next
;;;;;;;;;;;;;;;

(define (regenerate-with-template path [template-name empty] #:force [force #f]) 
  ; take full path or filename
  ; return full path of templated file
  
  (define source-path (->complete-path 
                       (if (pollen-source? path) 
                           path
                           (make-pollen-source-path path))))
  
  (define-values (source-dir source-name ignored) (split-path source-path))
  
  ; get body out of source file (to retrieve template name)
  ; use dynamic-rerequire to force refresh for dynamic-require, 
  ; otherwise it will cache
  ; parameterize needed because source files have relative requires
  (define file-was-reloaded-port (open-output-string))
  (parameterize ([current-directory source-dir]
                 [current-error-port file-was-reloaded-port])
    ; by default, rerequire reports reloads to error port.
    ; so capture this message to find out if anything was reloaded.
    (dynamic-rerequire source-path))
  
  (define file-was-reloaded? 
    (> (string-length (get-output-string file-was-reloaded-port)) 0))
  
  ; set template, regenerate, get data
  ; first, if no template name provided, look it up
  (when (or (empty? template-name) (not (file-exists? (build-path source-dir template-name))))
    ; get template name out of meta fields.
    ; todo: template file in body may not refer to a file that exists.
    ; todo: consider whether file-was-reloaded could change metas
    ; (because here, I'm retrieving them from existing source)
    
    ;;;;;;;;;;;;;;
    ;; todo: next
    ;;;;;;;;;;;;;;
    
    (define metas (dynamic-require source-path 'metas))
    (set! template-name (if (TEMPLATE_META_KEY . in? . metas)
                            (get metas TEMPLATE_META_KEY)
                            DEFAULT_TEMPLATE)))
  (define template-path (build-path source-dir template-name))
  ; refresh template (it might have its own p file)
  (regenerate template-path #:force force)
  
  ; calculate new path for generated file: 
  ; base from source + ext from template
  (define generated-path (build-path source-dir (add-ext (remove-ext source-name) (get-ext template-path))))
  
  ; do we need to refresh?
  (when (or force
            (not (file-exists? generated-path)) 
            (source-needs-refresh? source-path template-path)
            file-was-reloaded?)
    (store-refresh-in-mod-dates source-path template-path)
    
    ; Templates are part of the compile operation.
    ; Therefore no way to arbitrarily invoke template at run-time.
    ; This routine creates a new namespace and compiles the template within it.
    ; Todo: performance improvement would be to make a macro
    ; that pre-compiles all known templates into their own functions.
    ; then apply-template can either look for one of those functions,
    ; if the template exists,
    ; or if not found, use the eval technique.
    (define page-result
      ; parameterize current-directory to make file requires work
      (parameterize ([current-namespace (make-base-empty-namespace)]
                     [current-directory source-dir]
                     [current-output-port (open-output-nowhere)])
        (namespace-require 'racket) ; use namespace-require for FIRST require, then eval after
        (eval '(require (planet mb/pollen/template)) (current-namespace))
        ; import source into eval space, 
        ; automatically sets up main & metas & here
        (eval `(require ,(path->string source-name)) (current-namespace)) 
        (eval `(include-template #:command-char ,TEMPLATE_FIELD_DELIMITER ,template-name) (current-namespace))))
    
    
    (display-to-file #:exists 'replace page-result generated-path)
    (regenerated-message generated-path)))



;; regenerate files listed in a pmap file
(define/contract (regenerate-with-pmap pmap #:force [force #f])
  (pmap? . -> . void?)    
  ;; pass force parameter through 
  (for-each (λ(i) (regenerate i #:force force)) (all-pages pmap)))

;; todo: write test
