#lang racket/base
(require racket/list racket/path racket/port racket/system 
         racket/file racket/rerequire racket/contract)
(require "world.rkt" "tools.rkt" "pmap.rkt" "readability.rkt")

(module+ test (require rackunit))

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
(define/contract (store-refresh-in-mod-dates paths)
  ((listof path?) . -> . void?)
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
(define/contract (source-needs-refresh? paths)
  ((listof path?) . -> . boolean?)
  (or (not (paths . in? . mod-dates))  ; no stored mod date
      (not (equal? (map path->mod-date-value paths) (get mod-dates paths))))) ; data has changed

(module+ test 
  (reset-mod-dates)
  (let ([path (build-path (current-directory) (->path "regenerate.rkt"))])
    (store-refresh-in-mod-dates (list path))
    (check-false (source-needs-refresh? (list path)))
    (reset-mod-dates)
    (check-true (source-needs-refresh? (list path)))))


; helper functions for regenerate functions
;(define pollen-file-root (current-directory))

; complete pollen path =
;(build-path pollen-file-root f)


;; regenerate files listed in a pmap file
(define/contract (regenerate-with-pmap pmap)
  (pmap? . -> . void?)    
  (for-each regenerate (all-pages pmap)))

;; todo: write test


;; dispatches path to the right place
(define/contract (regenerate path #:force [force #f])
  (path? . -> . void?)
  (regenerating-message path)
  (let ([path (->complete-path path)])
    (cond
      [(pmap-source? path) (let ([pmap (dynamic-require path 'main)])
                             (regenerate-with-pmap pmap))]
      [(needs-preproc? path) (do-preproc path #:force force)]
      [(needs-template? path) (do-template path #:force force)])))

;; todo: write test



(define (regenerating-message path)
  (message "Regenerating:" (->string (file-name-from-path path))))

(define (do-preproc path #:force [force #f])
  ; set up preproc-in-path & preproc-out-path values
  (let-values 
      ([(preproc-in-path preproc-out-path) 
        (if (preproc-source? path) 
            (values path (make-preproc-out-path path))
            (values (make-preproc-in-path path) path))])    
    
    (when (and (file-exists? preproc-in-path) 
               (or force
                   (not (file-exists? preproc-out-path))
                   (source-needs-refresh? preproc-in-path)))
      (store-refresh-in-mod-dates preproc-in-path)
      ; use single quotes to escape spaces in pathnames
      (define command 
        (format "~a '~a' > '~a'" RACKET_PATH preproc-in-path preproc-out-path))
      ; discard output using open-output-nowhere
      (parameterize ([current-output-port (open-output-nowhere)])
        (system command))
      (regenerate-message preproc-out-path))))


(define (do-template path [template-name empty] #:force [force #f]) 
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
    (regenerate-message generated-path)))


(provide regenerate regenerate-all-files)