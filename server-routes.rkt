#lang racket/base
(require racket/list racket/contract racket/rerequire racket/file racket/format xml)
(require (only-in net/url url-query url->path url->string))
(require (only-in web-server/http/request-structs request-uri request-client-ip))
(require "world.rkt" "render.rkt" "readability.rkt" "predicates.rkt" "debug.rkt")

(module+ test (require rackunit))

;;; Routes for the server module
;;; separated out for ease of testing
;;; because it's tedious to start the server just to check a route.

(provide (all-defined-out))

;; extract main xexpr from a path
(define/contract (file->xexpr path #:render [wants-render #t])
  ((complete-path?) (#:render boolean?) . ->* . tagged-xexpr?)
  (when wants-render (render path))
  (dynamic-rerequire path) ; stores module mod date; reloads if it's changed
  (dynamic-require path 'main))

;; todo: rewrite this test, obsolete since filename convention changed
;;(module+ test
;;  (check-equal? (file->xexpr (build-path (current-directory) "tests/server-routes/foo.p") #:render #f) '(root "\n" "foo")))

;; read contents of file to string
;; just file->string with a render option
(define/contract (slurp path #:render [wants-render #t])
  ((complete-path?) (#:render boolean?) . ->* . string?) 
  (when wants-render (render path))
  (file->string path))

(module+ test
  (check-equal? (slurp (build-path (current-directory) "tests/server-routes/bar.html") #:render #f) "<html><body><p>bar</p></body></html>"))


;; add a wrapper to tagged-xexpr that displays it as monospaced text
;; for "view source"ish functions
;; takes either a string or an xexpr
(define/contract (format-as-code x)
  (xexpr? . -> . tagged-xexpr?)
  `(div ((style "white-space:pre-wrap;font-family:AlixFB,monospaced")) ,x))

(module+ test
  (check-equal? (format-as-code '(p "foo")) '(div ((style "white-space:pre-wrap;font-family:AlixFB,monospaced")) (p "foo"))))

;; server routes
;; these all produce an xexpr, which is handled upstream by response/xexpr

;; server route that returns html
;; todo: what is this for?
(define/contract (route-html path)
  (complete-path? . -> . xexpr?)
  (file->xexpr path))

;; server route that returns raw html, formatted as code
;; for viewing source without using "view source"
(define/contract (route-raw-html path)
  (complete-path? . -> . xexpr?)
  (format-as-code (slurp path #:render #f)))

;; todo: consolidate with function above, they're the same.
;; server route that shows contents of file on disk
(define/contract (route-source path)
  (complete-path? . -> . xexpr?)
  (format-as-code (slurp path)))


;; server route that returns xexpr (before conversion to html)
(define/contract (route-xexpr path)
  (complete-path? . -> . xexpr?)
  (format-as-code (~v (file->xexpr path))))



(define/contract (route-index)   
  (-> xexpr?)
  
  ;; This function generates the Pollen dashboard.
  ;; First, generate some lists of files.
  
  ;; get lists of files by mapping a filter function for each file type
  (define-values (pollen-files preproc-files ptree-files template-files)
    (let ([all-files-in-project-directory (directory-list pollen-project-directory)])
      (apply values 
             (map (λ(test) (filter test all-files-in-project-directory)) 
                  (list pollen-source? preproc-source? ptree-source? template-source?)))))
  
  ;; The actual post-preproc files may not have been generated yet
  ;; so calculate their names (rather than rely on directory list)
  (define post-preproc-files (map ->output-path preproc-files))
  
  ;; Make a combined list of preproc files and post-preproc file, in alphabetical order
  (define all-preproc-files (sort (append preproc-files post-preproc-files) 
                                  #:key path->string string<?))
  
  ;; calculate names of post-pollen files
  (define post-pollen-files (map ->output-path pollen-files))
  
  ;; Make a combined list of pollen files and post-pollen files, in alphabetical order
  (define all-pollen-files (sort (append pollen-files post-pollen-files) #:key path->string string<?))
  
  (define leftover-files (filter (λ(f) (and
                                        (not (equal? (->string f) "polcom")) ;todo: generalize this test
                                        (not ((->string f) . starts-with? . "."))
                                        (not (f . in? . all-pollen-files)))) 
                                 (directory-list pollen-project-directory)))
  
  (message leftover-files)
  
  ;; Utility function for making file rows
  (define (make-file-row file routes)
    ;; Utility function for making cells
    (define (make-link-cell type)
      (let* ([source (add-ext (remove-ext file) POLLEN_SOURCE_EXT)]
             [preproc-source (add-ext file POLLEN_PREPROC_EXT)]
             [file-string (->string file)]
             [name (case type 
                     ['direct file-string]
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
  
  (if (andmap empty? (list ptree-files all-pollen-files all-preproc-files template-files leftover-files))
      '(body "No files yet. Get to work!")
      `(body 
        (style ((type "text/css")) "td a { display: block; width: 100%; height: 100%; padding: 8px; }"
               "td:hover {background: #eee}")
        (table ((style "font-family:Concourse T3;font-size:115%"))
               ;; options for ptree files and template files
               ,@(map (λ(file) (make-file-row file '(raw))) (append leftover-files ptree-files template-files))
               
               ;; options for pollen files
               ,@(map (λ(file) (make-file-row file '(raw source xexpr force))) post-pollen-files)
               ;; options for preproc files
               ;; branching in λ is needed so these files can be interleaved on the list
               ,@(map (λ(file) (make-file-row file '(raw preproc-source))) post-preproc-files)))))


(define (get-query-value url key)
  ; query is parsed as list of pairs, key is symbol, value is string
  ; '((key . "value") ... )
  (let ([result (memf (λ(x) (equal? (car x) key)) (url-query url))])
    (and result (cdar result)))) ; second value of first result


; default route
(define (route-default req)  
  (define request-url (request-uri req))
  (define path (reroot-path (url->path request-url) pollen-project-directory))
  (define force (equal? (get-query-value request-url 'force) "true"))
  (with-handlers ([exn:fail? (λ(e) (message "Render is skipping" (url->string request-url) "because of error\n" (exn-message e)))])
    (render path #:force force)))