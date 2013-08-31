#lang racket/base
(require racket/list racket/contract racket/rerequire racket/file racket/format xml)
(require (only-in net/url url-query url->path))
(require (only-in web-server/http/request-structs request-uri))
(require "world.rkt" "regenerate.rkt" "readability.rkt" "predicates.rkt")

(module+ test (require rackunit))

;;; Routes for the server module
;;; separated out for ease of testing
;;; because it's tedious to start the server just to check a route.

(provide (all-defined-out))

;; extract main xexpr from a path
(define/contract (file->xexpr path #:regen [regen #t])
  ((complete-path?) (#:regen boolean?) . ->* . tagged-xexpr?)
  (when regen (regenerate path)) ; refresh path 
  (dynamic-rerequire path) ; stores module mod date; reloads if it's changed
  (dynamic-require path 'main))

(module+ test
  (check-equal? (file->xexpr (build-path (current-directory) "tests/server-routes/foo.p") #:regen #f) '(root "\n" "foo")))

;; read contents of file to string
;; just file->string with a regenerate option
(define/contract (slurp path #:regen [regen #t])
  ((complete-path?) (#:regen boolean?) . ->* . string?) 
  (when regen (regenerate path))
  (file->string path))

(module+ test
  (check-equal? (slurp (build-path (current-directory) "tests/server-routes/bar.html") #:regen #f) "<html><body><p>bar</p></body></html>"))


;; add a wrapper to tagged-xexpr that displays it as monospaced text
;; for "view source"ish functions
;; takes either a string or an xexpr
(define/contract (format-as-code x)
  (xexpr? . -> . tagged-xexpr?)
  `(div ((style "white-space:pre-wrap;font-family:AlixFB,monospaced")) ,x))

(module+ test
  (check-equal? (format-as-code '(p "foo")) '(div ((style "white-space:pre-wrap;font-family:AlixFB,monospaced")) (p "foo"))))

;; server routes
;; these all produce an xexpr, which is handled upstream by (response/xexpr x)

;; server route that returns html
;; todo: what is this for?
(define/contract (route-html path)
  (complete-path? . -> . xexpr?)
  (file->xexpr path))

;; server route that returns raw html, formatted as code
;; for viewing source without using "view source"
(define/contract (route-raw-html path)
  (complete-path? . -> . xexpr?)
  (format-as-code (slurp path #:regen #f)))

;; todo: consolidate with function above, they're the same.
;; server route that shows contents of file on disk
(define/contract (route-source path)
  (complete-path? . -> . xexpr?)
  (format-as-code (slurp path)))


;; server route that returns xexpr (before conversion to html)
(define/contract (route-xexpr path)
  (complete-path? . -> . xexpr?)
  (format-as-code (~v (file->xexpr path))))



(define/contract (route-index pollen-file-root)   
  (complete-path? . -> . xexpr?)
  
  ;; This function generates the Pollen dashboard.
  ;; First, generate some lists of files.
  
  ;; get lists of files by mapping a filter function for each file type
  (define-values (pollen-files preproc-files pmap-files template-files)
    (let ([all-files-in-project-directory (directory-list pollen-file-root)])
      (apply values 
             (map (λ(test) (filter test all-files-in-project-directory)) 
                  (list pollen-source? preproc-source? pmap-source? template-source?)))))
  
  ;; The actual post-preproc files may not have been generated yet
  ;; so calculate their names (rather than rely on directory list)
  (define post-preproc-files (map (λ(path) (remove-ext path)) preproc-files))
  
  ;; Make a combined list of preproc files and post-preproc file, in alphabetical order
  (define all-preproc-files (sort (append preproc-files post-preproc-files) 
                                  #:key path->string string<?))
  
  ;; calculate names of post-pollen files
  ;; todo: this isn't quite right. Assumes post-pollen file will have html extension.
  ;; not necessarily true (it will assume the extension of its template.)
  ;; But pulling out all the template extensions requires parsing all the files,
  ;; which is slow and superfluous, since we're trying to be lazy about rendering.
  (define post-pollen-files (map (λ(path) (add-ext (remove-ext path) 'html)) pollen-files))
  
  ;; Make a combined list of pollen files and post-pollen files, in alphabetical order
  (define all-pollen-files (sort (append pollen-files post-pollen-files) #:key path->string string<?))
  
  ;; Utility function for making file rows
  (define (make-file-row file routes)
    (define (make-link-cell type)
      (letrec ([source (add-ext (remove-ext file) POLLEN_SOURCE_EXT)]
               [preproc-source (add-ext file POLLEN_PREPROC_EXT)]
               [file-string (path->string file)]
               [name (case type 
                       ['direct (->string file-string)]
                       ['preproc-source "source"]
                       [else (->string type)])]
               [target (case type
                         ['direct name]
                         [(source xexpr) (format "/~a/~a" type source)]
                         ['preproc-source (format "/~a/~a" 'raw preproc-source)]
                         ['force (format "/~a?force=true" file-string)]
                         [else (format "/~a/~a" type file-string)])])
        `(td (a ((href ,target)) ,name))))
    `(tr ,(make-link-cell 'direct) ,@(map make-link-cell routes)))
  
  (if (andmap empty? (list pmap-files all-pollen-files all-preproc-files template-files))
      '(body "No files yet. Get to work!")
      `(body 
        (style ((type "text/css")) "td a { display: block; width: 100%; height: 100%; padding: 8px; }"
               "td:hover {background: #eee}")
        (table ((style "font-family:Concourse T3;font-size:115%"))
               ; options for pmap files and template files
               ,@(map (λ(file) (make-file-row file '(raw))) (append pmap-files template-files))
               ; options for pollen files
               ,@(map (λ(file) (make-file-row file '(raw source xexpr force))) post-pollen-files)
               ; options for preproc files
               ; branching in λ is needed so these files can be interleaved on the list
               ,@(map (λ(file) (make-file-row file '(raw preproc-source))) post-preproc-files)))))


(define (get-query-value url key)
  ; query is parsed as list of pairs, key is symbol, value is string
  ; '((key . "value") ... )
  (let ([result (memf (λ(x) (equal? (car x) key)) (url-query url))])
    (if result
        (cdar result) ; second value of first result
        result)))

; default route w/preproc support
(define (route-preproc path #:force force-value)
  (regenerate path #:force force-value))