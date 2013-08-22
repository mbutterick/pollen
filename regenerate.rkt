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
;; need list as input (rather than individual path)
;; because hash key needs to be a list
;; so it's convenient to use a rest argument
;; therefore, use function by just listing out the paths
(define/contract (store-refresh-in-mod-dates . rest-paths)
  (() #:rest (listof path?) . ->* . void?)
  (hash-set! mod-dates rest-paths (map path->mod-date-value rest-paths)))

(module+ test
  (reset-mod-dates)
  (store-refresh-in-mod-dates (build-path (current-directory) (->path "regenerate.rkt")))
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
  (store-refresh-in-mod-dates (build-path (current-directory) (->path "regenerate.rkt")))
  (reset-mod-dates)
  (check-true (= (len mod-dates) 0)))

;; how to know whether a certain combination of paths needs a refresh
;; use rest argument here so calling pattern matches store-refresh
(define/contract (mod-date-expired? . rest-paths)
  (() #:rest (listof path?) . ->* . boolean?)
  (or (not (rest-paths . in? . mod-dates))  ; no stored mod date
      (not (equal? (map path->mod-date-value rest-paths) (get mod-dates rest-paths))))) ; data has changed

(module+ test 
  (reset-mod-dates)
  (let ([path (build-path (current-directory) (->path "regenerate.rkt"))])
    (store-refresh-in-mod-dates path)
    (check-false (mod-date-expired? path))
    (reset-mod-dates)
    (check-true (mod-date-expired? path))))


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
      [(file-exists? path) (message "Regenerate: passing through" (->string (file-name-from-path path)))]
      [else (error "File not found:" (->string (file-name-from-path path)))])))

;; todo: write tests


;; todo: write contract & tests
(define (regenerating-message path)
  ;; you can actually stuff whatever string you want into path —
  ;; if it's not really a path, file-name-from-path won't choke
  (message "Regenerating:" (->string (file-name-from-path path))))

;; todo: write contract & tests
(define (regenerated-message path)
  (message "Regenerated:" (->string (file-name-from-path path))))


;; todo: these are misnamed. Not "complete"
(define (complete-preproc-source-path x)
  (define path (->path x))
  (if (preproc-source? path)
      path
      (make-preproc-source-path path)))

(define (complete-preproc-output-path x)
  (define path (->path x))
  (if (preproc-source? path)
      (make-preproc-output-path path)
      path))

(define/contract (regenerate-with-preproc x #:force [force #f])
  (((and/c pathish?
           (flat-named-contract 'file-exists
                                (λ(x) (file-exists? (complete-preproc-source-path x)))))) (#:force boolean?) . ->* . void?)
  
  
  ;; path might be either a preproc-source path or preproc-output path
  ;; figure out which, then compute the other
  (define source-path (complete-preproc-source-path x))
  (define output-path (complete-preproc-output-path x))
  
      ;; Three conditions under which we refresh:
      (if (or
           ;; 1) explicitly forced refresh
           force 
           ;; 2) output file doesn't exist (so it definitely won't appear in mod-dates)
           ;; also, this is convenient for development: 
           ;; you can trigger a refresh just by deleting the file
           (not (file-exists? output-path)) 
           ;; 3) file otherwise needs refresh (e.g., it changed)
           (mod-date-expired? source-path)) 
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
          (message "File is up to date:" (->string (file-name-from-path output-path)))))

;; todo: write tests


;; utility function for regenerate-with-template
(define/contract (handle-source-rerequire source-path)
  ((and/c path? file-exists?) . -> . boolean?)
  
  ;; dynamic-rerequire watches files to see if they change.
  ;; if so, then it reloads them.
  ;; therefore, it's useful in a development environment
  ;; because it reloads as needed, but otherwise not.
  
  (define-values (source-dir source-name _) (split-path source-path))
  ;; need to require source file (to retrieve template name, which is in metas)
  ;; but use dynamic-rerequire now to force refresh for dynamic-require later,
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

(define (complete-pollen-source-path x)
  (->complete-path (make-pollen-source-path (->path x))))

;; apply template
(define/contract (regenerate-with-template x [template-name #f] #:force [force #f]) 
  (((and/c pathish? 
           (flat-named-contract 'file-exists
                                (λ(x) (file-exists? (complete-pollen-source-path x))))))
   (path? #:force boolean?) . ->* . void?)
  
  ;; set up information about source
  (define source-path (complete-pollen-source-path x))
  ;; todo: this won't work with source files nested down one level
  (define-values (source-dir ignored also-ignored) (split-path source-path))
  
  ;; find out whether source had to be reloaded
  (define source-reloaded? (handle-source-rerequire source-path))
  
  ;; Then the rest: 
  ;; set the template, regenerate the source file with template, and catch the output.
  ;; 1) Set the template.
  (define template-path 
    (build-path source-dir
                ;; if template name provided and exists, use that
                (if (and template-name (file-exists? (build-path source-dir template-name)))
                    template-name
                    ;; Otherwise — try to get template name out of meta fields.
                    ;; todo: consider that template file in metas 
                    ;; may not refer to a file that exists.
                    ;; what then?
                    (let ([source-metas (dynamic-require source-path 'metas)])
                      (if (TEMPLATE_META_KEY . in? . source-metas)
                          (get source-metas TEMPLATE_META_KEY)
                          DEFAULT_TEMPLATE)))))
  
  ;; refresh template (it might have its own preprocessor file)
  (regenerate template-path #:force force)
  
  ;; calculate new path for generated file: base from source + ext from template
  (define output-path (make-pollen-output-path source-path (get-ext template-path)))
  
  ;; 2) Regenerate the source file with template, if needed.
  ;; Regenerate is expensive, so we avoid it when we can.
  ;; Four conditions where we regenerate:
  (if (or force ; a) it's explicitly demanded
          (not (file-exists? output-path)) ; b) output file does not exist
          ;; c) mod-dates indicates refresh is needed
          (mod-date-expired? source-path template-path) 
          ;; d) dynamic-rerequire indicates the source had to be reloaded
          source-reloaded?)
      (begin
        (store-refresh-in-mod-dates source-path template-path)
        (let ([page-result (render-source-with-template source-path template-path)])
          (display-to-file #:exists 'replace page-result output-path)
          (regenerated-message (file-name-from-path output-path))))
      (message "File is up to date:" (->string (file-name-from-path output-path)))))


(define/contract (render-source-with-template source-path template-path)
  (file-exists? file-exists? . -> . string?)
  
  ;; set up information about source and template paths
  ;; todo: how to write these without blanks?
  (define-values (source-dir source-name _) (split-path source-path))
  (define-values (___ template-name __) (split-path template-path))
  
  ;; Templates are part of the compile operation.
  ;; Therefore no way to arbitrarily invoke template at run-time.
  ;; This routine creates a new namespace and compiles the template within it.
  
  ;; parameterize current-directory to make file requires work
  ;; the below expression will evaluate to a string 
  ;; that represents the output of the operation.
  (parameterize ([current-namespace (make-base-empty-namespace)]
                 [current-directory source-dir]
                 [current-output-port (open-output-nowhere)])
    (namespace-require 'racket) ; use namespace-require for FIRST require, then eval after
    (eval '(require web-server/templates) (current-namespace))
    (eval '(require (planet mb/pollen/pmap)) (current-namespace))
    ;; import source into eval space. This sets up main & metas
    (eval `(require ,(path->string source-name)) (current-namespace)) 
    (eval `(include-template #:command-char ,TEMPLATE_FIELD_DELIMITER ,(->string template-name)) (current-namespace))))

;; regenerate files listed in a pmap file
(define/contract (regenerate-with-pmap pmap #:force [force #f])
  (pmap? . -> . void?)    
  ;; pass force parameter through 
  (for-each (λ(i) (regenerate i #:force force)) (all-pages pmap)))

;; todo: write test
