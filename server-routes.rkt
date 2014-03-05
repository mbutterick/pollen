#lang racket/base
(require racket/list racket/contract racket/rerequire racket/file racket/format xml racket/match racket/set racket/string racket/promise racket/path)
(require web-server/http/xexpr web-server/dispatchers/dispatch)
(require net/url)
(require web-server/http/request-structs)
(require web-server/http/response-structs)
(require 2htdp/image)
(require "world.rkt" "render.rkt" sugar txexpr "predicates.rkt" "debug.rkt" "ptree.rkt")

(module+ test (require rackunit))

;;; Routes for the server module
;;; separated out for ease of testing
;;; because it's tedious to start the server just to check a route.

(provide route-dashboard route-xexpr route-default route-404 route-in route-out)

(define (body-wrapper content-xexpr)
  `(html 
    (head
     (meta ([charset "UTF-8"]))
     (link ([rel "stylesheet"] 
            [type "text/css"] 
            [href ,(format "/~a" world:dashboard-css)])))
    (body
     ,content-xexpr (div ((id "pollen-logo"))))))

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
  (message "request:" (string-replace url-string world:dashboard-name " dashboard")
           (if (not (equal? client localhost-client)) (format "from ~a" client) "")))

;; pass string args to route, then
;; package route into right format for web server
;; todo: fix inbound contrfact to be proc with (path? . -> . xexpr?)
;; todo: fix outbound contract to be proc with (request? #:rest args . -> . response?)
(define/contract (route-wrapper route-proc)
  (procedure? . -> . procedure?)
  (λ(req . string-args) 
    (logger req) 
    (define path (apply build-path (world:current-project-root) (flatten string-args)))
    (response/xexpr (route-proc path))))


;; extract main xexpr from a path
(define/contract (file->xexpr path #:render [wants-render #t])
  ((complete-path?) (#:render boolean?) . ->* . txexpr?)
  (when wants-render (render-for-dev-server path))
  (dynamic-rerequire path) ; stores module mod date; reloads if it's changed
  (dynamic-require path world:main-pollen-export))

;; todo: rewrite this test, obsolete since filename convention changed
;;(module+ test
;;  (check-equal? (file->xexpr (build-path (current-directory) "tests/server-routes/foo.p") #:render #f) '(root "\n" "foo")))

;; read contents of file to string
;; just file->string with a render option
(define/contract (slurp path #:render [wants-render #t])
  ((complete-path?) (#:render boolean?) . ->* . string?) 
  (when wants-render (render-for-dev-server path))
  (file->string path))

(module+ test
  (check-equal? (slurp (build-path (current-directory) "tests/server-routes/bar.html") #:render #f) "<html><body><p>bar</p></body></html>"))


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
  (define relative-path (->string (find-relative-path (world:current-project-root) path)))
  (define img-url (format "/~a" relative-path))
  `(div  
    (p "filename =" ,(->string relative-path))
    (p "size = " ,(bytecount->string (file-size path)))
    ,@(when/splice (not (equal? (get-ext path) "svg"))
                   `(p "width = " ,(->string (image-width img)) " " 
                       "height = " ,(->string (image-height img))))
    (a ((href ,img-url)) (img ((style "width:100%;border:1px solid #eee")(src ,img-url))))))

(require file/unzip)
(define (handle-zip-path p)
  (pathish? . -> . xexpr?)
  (define path (->path p))
  (define relative-path (->string (find-relative-path (world:current-project-root) path)))
  (define ziplist (zip-directory-entries (read-zip-directory path)))
  `(div  
    (p "filename =" ,(->string relative-path))
    (p "size = " ,(bytecount->string (file-size path)))
    (ul ,@(map (λ(i) `(li ,(~a i))) ziplist))))




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
(define (dashboard dashfile)
  (define dir (get-enclosing-dir dashfile))
  (define (in-project-root?)
    (directories-equal? dir (world:current-project-root)))
  (define parent-dir (and (not (in-project-root?)) (get-enclosing-dir dir)))
  (define empty-cell (cons #f #f))
  (define (make-link-cell href+text)
    (match-define (cons href text) href+text) 
    (filter-not void? `(td ,(when text 
                              (if href 
                                  `(a ((href ,href)) ,text)
                                  text)))))
  (define (make-parent-row) 
    (if parent-dir
        (let* ([url-to-parent-dashboard (format "/~a" (find-relative-path (world:current-project-root) (build-path parent-dir world:dashboard-name)))]
               [url-to-parent (string-replace url-to-parent-dashboard world:dashboard-name "")])
          `(tr (th ((colspan "3")) (a ((href ,url-to-parent-dashboard)) ,(format "up to ~a" url-to-parent))))) 
        `(tr (th ((colspan "3")(class "root")) "Pollen root"))))
  
  (define (make-path-row fn)
    (define filename (->string fn))
    (define (file-in-dir? fn) (file-exists? (build-path dir fn)))
    (define possible-sources 
      (if (directory-exists? fn)
          empty ;; folders don't have source files
          (filter file-in-dir? (list (->preproc-source-path filename) (->markup-source-path filename) (->null-source-path filename) (->scribble-source-path filename)))))
    (define source (and (not (empty? possible-sources)) (->string (car possible-sources))))
    `(tr ,@(map make-link-cell 
                (append (list                          
                         (cond ; main cell
                           [(directory-exists? (build-path dir filename)) ; links subdir to its dashboard
                            (cons (format "~a/~a" filename world:dashboard-name) (format "~a/" filename))]
                           [(and source (equal? (get-ext source) "scrbl")) 
                            (cons #f `(a ((href ,filename)) ,filename (span ((class "file-ext")) " (from " ,source ")")))]
                           [source (cons #f `(a ((href ,filename)) ,filename (span ((class "file-ext")) "." ,(get-ext source))))]
                           [else   (cons filename filename)])
                         
                         (cond ; in cell
                           [source  (cons (format "in/~a" source) "in")]
                           [(or (ptree-source? filename) (sourceish? filename))  (cons (format "in/~a" filename) "in")]
                           [else empty-cell])
                         
                         (cond ; out cell 
                           [(directory-exists? (build-path dir filename)) (cons #f #f)]
                           [(ptree-source? filename) empty-cell]
                           [else (cons (format "out/~a" filename) "out")]))))))
  
  (define (ineligible-path? x) (or (not (visible? x)) (member x world:reserved-paths)))  
  
  (define (unique-sorted-output-paths xs)
    (define output-paths (map ->output-path xs))
    (define (unique-members xs) (set->list (list->set xs)))
    (define all-paths (unique-members output-paths))
    (define path-is-directory? (λ(f) (directory-exists? (build-path dir f))))
    (define subdirectories (filter path-is-directory? all-paths))
    (define files (filter-not path-is-directory? all-paths))
    (define ptree-sources (filter ptree-source? files))
    (define other-files (filter-not ptree-source? files))
    (define (sort-names xs) (sort xs #:key ->string string<?))
    ;; put subdirs in list ahead of files (so they appear at the top)
    (append (sort-names subdirectories) (sort-names ptree-sources) (sort-names other-files)))
  
  (define project-paths (filter-not ineligible-path? (if (file-exists? dashfile)
                                                         (map ->path (ptree->list (file->ptree dashfile)))
                                                         (unique-sorted-output-paths (directory-list dir)))))
  
  (body-wrapper
   `(table 
     ,@(cons (make-parent-row) 
             (map make-path-row project-paths)))))

(define route-dashboard (route-wrapper dashboard))


(define (get-query-value url key)
  ; query is parsed as list of pairs, key is symbol, value is string
  ; '((key . "value") ... )
  (let ([result (memf (λ(x) (equal? (car x) key)) (url-query url))])
    (and result (cdar result))))


(define/contract (req->path req)
  (request? . -> . path?)
  (reroot-path (url->path (request-uri req)) (world:current-project-root)))

;; default route
(define (route-default req)  
  (logger req)
  (define force (equal? (get-query-value (request-uri req) 'force) "true"))
  (render-for-dev-server (req->path req) #:force force)
  (next-dispatcher))


;; 404 route
(define/contract (route-404 req)
  (request? . -> . response?)
  (define error-text (format "route-404: Can't find ~a" (->string (req->path req))))
  (message error-text)
  (response/xexpr `(html ,error-text)))



;; server route that returns xexpr (before conversion to html)
(define/contract (xexpr path)
  (complete-path? . -> . xexpr?)
  (format-as-code (~v (file->xexpr path))))

(define route-xexpr (route-wrapper xexpr))
