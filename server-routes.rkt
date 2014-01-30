#lang racket/base
(require racket/list racket/contract racket/rerequire racket/file racket/format xml racket/match racket/set racket/string racket/promise racket/path)
(require web-server/http/xexpr web-server/dispatchers/dispatch)
(require net/url)
(require web-server/http/request-structs)
(require web-server/http/response-structs)
(require "world.rkt" "render.rkt" "readability.rkt" "predicates.rkt" "debug.rkt")

(module+ test (require rackunit))

;;; Routes for the server module
;;; separated out for ease of testing
;;; because it's tedious to start the server just to check a route.

(provide route-dashboard route-xexpr route-default route-404 route-in route-out)

(define (body-wrapper content-xexpr)
  `(html 
    (head
     (meta ((charset "UTF-8")))
     (link ((rel "stylesheet") 
            (type "text/css") 
            (href ,(format "/~a" DASHBOARD_CSS)))))
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
  (define url-string (url->string (request-uri req)))
  (message "Request:" (string-replace url-string DASHBOARD_NAME " dashboard")
           "from" (if (equal? client "::1") "localhost" client)))

;; pass string args to route, then
;; package route into right format for web server
;; todo: fix inbound contrfact to be proc with (path? . -> . xexpr?)
;; todo: fix outbound contract to be proc with (request? #:rest args . -> . response?)
(define/contract (route-wrapper route-proc)
  (procedure? . -> . procedure?)
  (λ(req . string-args) 
    (logger req) 
    (define path (apply build-path PROJECT_ROOT (flatten string-args)))
    (response/xexpr (route-proc path))))


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
  (body-wrapper `(tt ,x)))


(define/contract (make-binary-info-page path)
  (complete-path? . -> . xexpr?)
  (cond
    [((get-ext path) . in? . '("gif" "jpg" "jpeg" "png")) 
     `(div
       (img ((src ,(path->string path))))
       (p "path =" ,(path->string path)))]
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
    [(has-binary-ext? path) (make-binary-info-page path)]
    [else (format-as-code (slurp path #:render #t))]))
(define route-out (route-wrapper out))


;; dashboard route
(define (dashboard dashfile)
  (define dir (get-enclosing-dir dashfile))
  (define (in-project-root?)
    (directories-equal? dir PROJECT_ROOT))
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
        (let* ([url-to-parent-dashboard (format "/~a" (find-relative-path PROJECT_ROOT (build-path parent-dir DASHBOARD_NAME)))]
               [url-to-parent (string-replace url-to-parent-dashboard DASHBOARD_NAME "")])
          `(tr (th ((colspan "3")) (a ((href ,url-to-parent-dashboard)) ,(format "up to ~a" url-to-parent))))) 
        `(tr (th ((colspan "3")(class "root")) "Pollen root"))))
  
  (define (make-path-row fn)
    (define filename (->string fn))
    (define (file-in-dir? fn) (file-exists? (build-path dir fn)))
    (define possible-sources 
      (if (directory-exists? fn)
          empty ;; folders don't have source files
          (filter file-in-dir? (list (->preproc-source-path filename) (->pollen-source-path filename)))))
    (define source (and (not (empty? possible-sources)) (->string (car possible-sources))))
    `(tr ,@(map make-link-cell 
                (append (list                          
                         (cond ; main cell
                           [(directory-exists? (build-path dir filename)) ; links subdir to its dashboard
                            (cons (format "~a/~a" filename DASHBOARD_NAME) (format "~a/" filename))]
                           [source (cons #f `(a ((href ,filename)) ,filename (span ((class "file-ext")) "." ,(get-ext source))))]
                           [else   (cons filename filename)])
                         
                         (cond ; in cell
                           [(has-ext? filename POLLEN_TREE_EXT) (cons (format "in/~a" filename) "ptree")]
                           [source  (cons (format "in/~a" source) "in")]
                           [else empty-cell])
                         
                         (cond ; out cell 
                           [(directory-exists? (build-path dir filename)) (cons #f #f)]
                           [(has-ext? filename POLLEN_TREE_EXT) empty-cell]
                           [else (cons (format "out/~a" filename) "out")]))))))
  
  (define (ineligible-path? x) (or (not (visible? x)) (member x RESERVED_PATHS)))  
  (define project-paths (filter-not ineligible-path? (directory-list dir)))
  
  (define (unique-sorted-output-paths xs)
    (define output-paths (map ->output-path xs))
    (define (unique-members xs) (set->list (list->set xs)))
    (define all-paths (unique-members output-paths))
    (define built-directory-exists? (λ(f) (directory-exists? (build-path dir f))))
    (define subdirectories (filter built-directory-exists? all-paths))
    (define files (filter-not built-directory-exists? all-paths))
    (define (sort-names xs) (sort xs #:key ->string string<?))
    ;; put subdirs in list ahead of files (so they appear at the top)
    (append (sort-names subdirectories) (sort-names files)))
  
  (body-wrapper
   `(table 
     ,@(cons (make-parent-row) 
             (map make-path-row (unique-sorted-output-paths project-paths))))))

(define route-dashboard (route-wrapper dashboard))


(define (get-query-value url key)
  ; query is parsed as list of pairs, key is symbol, value is string
  ; '((key . "value") ... )
  (let ([result (memf (λ(x) (equal? (car x) key)) (url-query url))])
    (and result (cdar result))))


(define/contract (req->path req)
  (request? . -> . path?)
  (reroot-path (url->path (request-uri req)) PROJECT_ROOT))

;; default route
(define (route-default req)  
  (logger req)
  (define force (equal? (get-query-value (request-uri req) 'force) "true"))
  (with-handlers ([exn:fail? (λ(e) (message "Render is skipping" (url->string (request-uri req)) "because of error\n" (exn-message e)))])
    (render (req->path req) #:force force))
  (next-dispatcher))


;; error route
(define/contract (route-404 req)
  (request? . -> . response?)
  (define error-text (format "Can't find ~a" (->string (req->path req))))
  (message error-text)
  ;`(html ,(slurp (build-path SERVER_EXTRAS_DIR "404.html")))
  (response/xexpr `(html ,error-text)))


;; server route that returns xexpr (before conversion to html)
(define/contract (xexpr path)
  (complete-path? . -> . xexpr?)
  (format-as-code (~v (file->xexpr path))))

(define route-xexpr (route-wrapper xexpr))

(module+ main
  (parameterize ([current-directory (build-path (current-directory) "foobar")])
    (reset-project-root)
    (message PROJECT_ROOT)
    (dashboard (build-path "poldash.html"))))