#lang racket/base
(require racket/list racket/contract racket/file racket/format xml racket/match racket/set racket/string racket/promise racket/path)
(require web-server/http/xexpr web-server/dispatchers/dispatch)
(require net/url)
(require web-server/http/request-structs)
(require web-server/http/response-structs)
(require web-server/http/redirect)
(require 2htdp/image)
(require "../setup.rkt" "../render.rkt" sugar sugar/unstable/string sugar/unstable/misc sugar/unstable/container txexpr/base "file-utils.rkt" "debug.rkt" "../pagetree.rkt" "../cache.rkt")

(module+ test (require rackunit))

;;; Routes for the server module
;;; separated out for ease of testing
;;; because it's tedious to start the server just to check a route.

(provide route-dashboard route-default route-404 route-in route-out route-index)

(define (response/xexpr+doctype xexpr)
  (response/xexpr #:preamble #"<!DOCTYPE html>" xexpr))

(define (body-wrapper  #:title [title #f] . content-xexpr)
  `(html 
    (head
     (title ,(if title title "Pollen"))
     (meta ([charset "UTF-8"]))
     (link ([rel "stylesheet"] 
            [type "text/css"] 
            [href ,(format "/~a" (setup:dashboard-css))])))
    (body
     ,@content-xexpr (div ((id "pollen-logo"))))))

;; to make dummy requests for debugging
(define/contract (string->request u)
  (string? . -> . request?)
  (make-request #"GET" (string->url u) empty
                (delay empty) #f "1.2.3.4" 80 "4.3.2.1"))


;; print message to console about a request
(define/contract (logger req)
  (request? . -> . void?) 
  (define client (request-client-ip req))
  (define localhost-client "::1")
  (define url-string (url->string (request-uri req)))
  (when (not (ends-with? url-string "favicon.ico"))
    (message "request:" (if (regexp-match #rx"/$" url-string)
                            (string-append url-string " directory default page")
                            (string-replace url-string (setup:main-pagetree) " dashboard"))
             (if (not (equal? client localhost-client)) (format "from ~a" client) ""))))

;; pass string args to route, then
;; package route into right format for web server
;; todo: fix inbound contrfact to be proc with (path? . -> . xexpr?)
;; todo: fix outbound contract to be proc with (request? #:rest args . -> . response?)
(define/contract (route-wrapper route-proc)
  (procedure? . -> . procedure?)
  (λ (req . string-args) 
    (logger req)
    ;; `flatten` here because servlet's route matcher might send a list of lists
    ;; for "before and after" matches, like `((string-arg) ... "in" (string-arg) ...)`
    (define path (apply build-path (current-project-root) (flatten string-args)))
    (response/xexpr+doctype (route-proc path))))



;; read contents of file to string
;; just file->string with a render option
(define/contract (slurp path #:render [wants-render #t])
  ((complete-path?) (#:render boolean?) . ->* . string?) 
  (when wants-render (render-from-source-or-output-path path))
  (file->string path))


;; add a wrapper to txexpr that displays it as monospaced text
;; for "view source"ish functions
;; takes either a string or an xexpr
(define/contract (format-as-code x)
  (xexpr? . -> . txexpr?)
  (body-wrapper `(tt ,x)))


(define (handle-image-path p)
  (pathish? . -> . xexpr?)
  (define path (->complete-path p))
  (define img (bitmap/file path))
  (define relative-path (->string (find-relative-path (current-project-root) path)))
  (define img-url (format "/~a" relative-path))
  `(div  
    (p "filename =" ,(->string relative-path))
    (p "size = " ,(bytecount->string (file-size path)))
    ,@(if (not (equal? (get-ext path) "svg"))
          `(p "width = " ,(->string (image-width img)) " " 
              "height = " ,(->string (image-height img)))
          "")
    (a ((href ,img-url)) (img ((style "width:100%;border:1px solid #eee")(src ,img-url))))))

(require file/unzip)
(define (handle-zip-path p)
  (pathish? . -> . xexpr?)
  (define path (->path p))
  (define relative-path (->string (find-relative-path (current-project-root) path)))
  (define ziplist (zip-directory-entries (read-zip-directory path)))
  `(div  
    (p "filename =" ,(->string relative-path))
    (p "size = " ,(bytecount->string (file-size path)))
    (ul ,@(map (λ (i) `(li ,(~a i))) ziplist))))




(define/contract (make-binary-info-page p)
  (pathish? . -> . xexpr?)
  (define path (->complete-path p))
  (cond
    [((get-ext path) . in? . '("gif" "jpg" "jpeg" "png" "svg")) 
     (handle-image-path path)]
    [((get-ext path) . in? . '("zip")) (handle-zip-path path)]
    [else '(p "We got some other kind of binary file.")]))

;; server routes
;; these all produce an xexpr, which is handled upstream by response/xexpr

;; server routes that show result, formatted as code
;; route-in just gets file from disk; route-out renders it first
(define/contract (in path)
  (complete-path? . -> . xexpr?)
  (format-as-code (slurp path #:render #f)))
(define route-in (route-wrapper in))

(define/contract (out path)
  (complete-path? . -> . xexpr?)
  (cond
    [(or (has-binary-ext? path) (sourceish? path)) (make-binary-info-page path)]
    [else (format-as-code (slurp path #:render #t))]))
(define route-out (route-wrapper out))


;; dashboard route
(define (dashboard dashboard-ptree)
  (define dashboard-dir (dirname dashboard-ptree))
  (define (in-project-root?)
    (directories-equal? dashboard-dir (current-project-root)))
  (define parent-dir (and (not (in-project-root?)) (dirname dashboard-dir)))
  (define empty-cell (cons #f #f))
  (define (make-link-cell href+text)
    (match-define (cons href text) href+text) 
    (filter-not void? `(cell ,(when text 
                              (if href 
                                  `(a ((href ,href)) ,text)
                                  text)))))
  
  (define (make-parent-row)
    (define title (string-append "Project root" (if (equal? (current-project-root) dashboard-dir) (format " = ~a" dashboard-dir) "")))
    (define dirs (cons title (if (not (equal? (current-project-root) dashboard-dir))
                                 (explode-path (find-relative-path (current-project-root) dashboard-dir))
                                 null)))
    (define dirlinks (cons "/" (map (λ (ps) (format "/~a/" (apply build-path ps)))  
                                    (for/list ([i (in-range (length (cdr dirs)))])
                                              (take (cdr dirs) (add1 i))))))
    `(row (heading ((colspan "3")) ,@(add-between (map (λ (dir dirlink) `(a ((href ,(format "~a~a" dirlink (setup:main-pagetree)))) ,(->string dir))) dirs dirlinks) "/"))))
  
  (define (make-path-row filename source indent-level)
    `(row ,@(map make-link-cell 
                (append (list                          
                         (let ([main-cell (cond ; main cell
                                            [(directory-exists? (build-path dashboard-dir filename)) ; links subdir to its dashboard
                                             (cons (format "~a/~a" filename (setup:main-pagetree)) (format "~a/" filename))]
                                            [(and source (equal? (get-ext source) "scrbl")) ; scribble source
                                             (cons #f `(a ((href ,filename)) ,filename (span ((class "file-ext")) " (from " ,(->string (find-relative-path dashboard-dir source)) ")")))]
                                            [source ; ordinary source. use remove-ext because source may have escaped extension in it
                                             (define source-first-ext (get-ext source))
                                             (define source-minus-ext (unescape-ext (remove-ext source)))
                                             (define source-second-ext (get-ext source-minus-ext))
                                             (cond ; multi source. expand to multiple output files.
                                               [(and source-second-ext (equal? source-second-ext (->string (setup:poly-source-ext (->complete-path source)))))
                                                (define source-base (remove-ext source-minus-ext))
                                                (define output-names (map (λ (ext) (->string (add-ext source-base ext))) (setup:poly-targets (->complete-path source))))
                                                (cons #f `(div ,@(map (λ (on) `(a ((href ,on)) ,on (span ((class "file-ext")) "." ,source-first-ext ,(format " (from ~a)" (->string (find-relative-path dashboard-dir source)))))) output-names)))]
                                               [else
                                                (define extra-row-string
                                                  (if (equal? source-minus-ext (remove-ext source)) ; escaped and unescaped versions are equal
                                                      "" ; no extra string needed
                                                      (format " (from ~a)" (->string (find-relative-path dashboard-dir source)))))
                                                
                                                (cons #f `(a ((href ,filename)) ,(->string source-minus-ext) (span ((class "file-ext")) "." ,source-first-ext ,extra-row-string)))])]
                                            [else ; other non-source file
                                             (cons filename filename)])])
                           
                           (cons (car main-cell)
                                 (let* ([cell-content (cdr main-cell)]
                                       [indent-padding (+ 1 indent-level)]
                                       [padding-attr `(class ,(format "indent_~a" indent-padding))])
                                   (cond
                                     [(string? cell-content) `(span (,padding-attr) ,cell-content)]
                                     [(txexpr? cell-content)
                                      ;; indent link text by depth in pagetree
                                      `(,(get-tag cell-content) ,(cons padding-attr (get-attrs cell-content)) ,@(get-elements cell-content))]
                                     [else (error 'make-path-row (format "mysterious cell data: ~v" cell-content))]))))
                         
                         (cond ; 'in' cell
                           [source  (cons (format "in/~a" source) "in")]
                           [(or (pagetree-source? filename) (sourceish? filename))  (cons (format "in/~a" filename) "in")]
                           [else empty-cell])
                         
                         (cond ; 'out' cell 
                           [(directory-exists? (build-path dashboard-dir filename)) (cons #f #f)]
                           [(pagetree-source? filename) empty-cell]
                           [else (cons (format "out/~a" filename) "out")]))))))
  
  (define (ineligible-path? x) (member x (setup:paths-excluded-from-dashboard)))
  
  (define directory-pagetree (with-handlers ([exn:fail:contract? (λ _ (directory->pagetree dashboard-dir))])
                               (cached-doc (->path dashboard-ptree))))
  
  (define project-paths (filter-not ineligible-path? (map ->path (pagetree->list directory-pagetree))))
  
  (define (directory-pagetree-depth node)
    (let loop ([node node][depth 0])
      (define pn (parent node directory-pagetree))
      (if pn
          (loop pn (add1 depth))
          depth)))
  
  (apply body-wrapper #:title (format "~a" dashboard-dir)
                (cons (make-parent-row) 
                          (cond
                            [(not (null? project-paths))
                             (define path-source-pairs
                               (map
                                (λ (p) (define source
                                        (let ([possible-source (get-source (build-path dashboard-dir p))])
                                          (and possible-source (->string (find-relative-path dashboard-dir possible-source)))))
                                  (cons p source))
                                project-paths))
                             
                             (define-values (reversed-unique-path-source-pairs seen-paths) ; delete pairs with duplicate sources
                               (for/fold ([psps empty][seen-source-paths empty])
                                         ([psp (in-list path-source-pairs)])
                                 (define source-path (cdr psp))
                                 (if (and source-path (member source-path seen-source-paths))
                                     (values psps seen-source-paths) ; skip the pair
                                     (values (cons psp psps) (cons source-path seen-source-paths)))))
                             
                             (define unique-path-source-pairs (reverse reversed-unique-path-source-pairs))
                             (define filenames (map (compose1 ->string car) unique-path-source-pairs))
                             (define sources (map cdr unique-path-source-pairs))
                             (define indent-levels (map directory-pagetree-depth filenames))
                             (parameterize ([current-directory dashboard-dir])
                               (map make-path-row filenames sources indent-levels))]
                            [else (list '(row (cell ((class "no-files")) "No files yet in this directory") (td) (td)))]))))

(define route-dashboard (route-wrapper dashboard))


(define (get-query-value url key)
  ; query is parsed as list of pairs, key is symbol, value is string
  ; '((key . "value") ... )
  (let ([result (memf (λ (x) (equal? (car x) key)) (url-query url))])
    (and result (cdar result))))


(define/contract (req->path req)
  (request? . -> . path?)
  (define base (current-project-root))
  (define file (url->path (request-uri req)))
  (if (eq? (system-path-convention-type) 'windows)
      (build-path base file) ; because url->path returns a relative path for 'windows
      (reroot-path file base))) ; and a complete path for 'unix

;; default route
(define (route-default req)  
  (logger req)
  (render-from-source-or-output-path (req->path req))
  (next-dispatcher))

;; index route
(define (route-index req . string-args)
  (logger req)
  (or (for*/first ([index-dir (in-value (simplify-path (req->path req)))]
                   [possible-idx-page (in-list (setup:index-pages index-dir))]
                   [possible-idx-path (in-value (build-path index-dir possible-idx-page))]
                   [_ (in-value (render-from-source-or-output-path possible-idx-path))]
                   #:when (file-exists? possible-idx-path))
                  (redirect-to (path->string (find-relative-path index-dir possible-idx-path)) temporarily))
      (route-404 req)))

;; 404 route
(define/contract (route-404 req)
  (request? . -> . response?)
  (define missing-path-string (path->string (simplify-path (req->path req))))
  (message (format "route-404: Can't find ~a" missing-path-string))
  (response/xexpr+doctype
   `(html 
     (head (title "404 error") (link ((href "/error.css") (rel "stylesheet"))))
     (body (div ((class "section")) (div ((class "title")) "404 error") (p ,(format "~v" missing-path-string) " was not found"))))))
