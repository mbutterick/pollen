#lang racket/base
(require racket/list racket/contract racket/rerequire racket/file racket/format)
(require (only-in net/url url-query url->path))
(require (only-in web-server/http/request-structs request-uri))
(require "world.rkt" "regenerate.rkt" "readability.rkt" "predicates.rkt")

(module+ test (require rackunit))

;;; Routes for the server module
;;; separated out for ease of testing
;;; because it's tedious to start the server just to check a route.

(provide (all-defined-out))


(define/contract (file->xexpr path)
  (complete-path? . -> . tagged-xexpr?)
  (regenerate path)
  (dynamic-rerequire path)
  (dynamic-require path 'main))


(define/contract (slurp path #:regenerate? [regenerate? #t])
  (complete-path? . -> . string?) 
  (when regenerate? (regenerate path))
  (file->string path))

(define/contract (format-as-code tx)
  (tagged-xexpr? . -> . tagged-xexpr?)
  `(div ((style "white-space:pre-wrap;font-family:AlixFB,monospaced")) ,tx))


(define/contract (route-html path)
  (complete-path? . -> . tagged-xexpr?)
  (file->xexpr path))

(define/contract (route-raw-html path)
  (complete-path? . -> . tagged-xexpr?)
  (format-as-code (slurp path)))

(define/contract (route-xexpr path)
  (complete-path? . -> . tagged-xexpr?)
  (format-as-code (~v (file->xexpr path))))

(define/contract (route-source path)
  (complete-path? . -> . tagged-xexpr?)
  (format-as-code (slurp path #:regenerate? #f)))


(define/contract (route-index pollen-file-root)   
  ((and/c path? complete-path?) . -> . tagged-xexpr?)
  ; set up filter functions by mapping a function-maker for each file type
  (define-values (pollen-file? preproc-file? pmap-file?)
    (apply values (map (λ(ext)(λ(f)(has-ext? f ext))) (list POLLEN_SOURCE_EXT POLLEN_PREPROC_EXT POLLEN_MAP_EXT))))
  (define (template-file? x)
    (define-values (dir name ignore) (split-path x))
    (equal? (get (->string name) 0) TEMPLATE_FILE_PREFIX))
  ; get lists of files by mapping a filter function for each file type
  (define-values (pollen-files preproc-files pmap-files template-files)
    (apply values (map (λ(test) (filter test (directory-list pollen-file-root))) (list pollen-file? preproc-file? pmap-file? template-file?))))
  ; the actual post-p files may not have been generated yet
  (define post-preproc-files (map (λ(path) (remove-ext path)) preproc-files))
  ; make a combined list of p-files and post-p files
  (define all-preproc-files (sort (append preproc-files post-preproc-files) #:key path->string string<?))
  
  (define post-pollen-files (map (λ(path) (add-ext (remove-ext path) 'html)) pollen-files))
  (define all-pollen-files (sort (append pollen-files post-pollen-files) #:key path->string string<?))
  
  
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