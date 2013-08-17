#! /Applications/Racket/bin/racket
#lang web-server
(require web-server/servlet-env)
(require web-server/dispatch web-server/dispatchers/dispatch)
(require racket/rerequire)
(require xml)
(require xml/path)
(require "tools.rkt" "world.rkt" "regenerate.rkt" "map.rkt")

(displayln "Pollen server starting...")

(define pollen-file-root (current-directory))

(define-values (start url)
  (dispatch-rules
   [("start") route-index]
   [("source" (string-arg)) route-source]
   [("xexpr" (string-arg)) route-xexpr]
   [("raw" (string-arg)) route-raw-html]
   [("html" (string-arg)) route-html]
   [else route-preproc]))


(define (get-query-value url key)
  ; query is parsed as list of pairs, key is symbol, value is string
  ; '((key . "value") ... )
  (let ([result (memf (λ(x) (equal? (car x) key)) (url-query url))])
    (if result
        (cdar result) ; second value of first result
        result)))

; default route w/preproc support
(define (route-preproc req)
  ; because it's the "else" route, can't use string-arg matcher
  ; so extract the path manually
  (define path 
    (reroot-path (url->path (request-uri req)) pollen-file-root))
  (define force-value (get-query-value (request-uri req) 'force))
  (regenerate path #:force force-value)
  ; serve path
  (next-dispatcher))


(define (slurp filename #:regenerate? [regenerate? #t])
  (define path (build-path pollen-file-root filename))
  (when regenerate?
    (regenerate path))
  (file->string path))

(define (file->xexpr filename)
  (define path (build-path pollen-file-root filename))
  (regenerate path)
  (dynamic-rerequire path)
  (define main (dynamic-require path 'main))
  main)


(define (format-as-code data)
  `(div ((style "white-space:pre-wrap;font-family:AlixFB,monospaced")) ,data))

(define (route-source req filename)
  (response/xexpr (format-as-code (slurp filename #:regenerate? #f))))

(define (route-xexpr req filename)
  (response/xexpr (format-as-code (~v (file->xexpr filename)))))

(define (route-raw-html req filename)
  (response/xexpr (format-as-code (slurp filename))))

(define (route-html req filename)
  (response/xexpr (file->xexpr filename)))

(define (route-index req)   
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
      (response/xexpr '(body "No files yet. Get to work!"))
      (response/xexpr 
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
                ,@(map (λ(file) (make-file-row file '(raw preproc-source))) post-preproc-files))))))


(displayln "Ready to rock")

(serve/servlet start
               #:port 8080
               #:listen-ip #f
               #:servlet-regexp #rx"" ; respond to top level
               #:command-line? #t
               #:extra-files-paths (list (build-path (current-directory)))
               ;               #:server-root-path (current-directory)
               )

