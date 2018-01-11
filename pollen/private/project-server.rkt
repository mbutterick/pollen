#lang web-server/base

(require racket/list
         racket/path
         racket/match
         setup/collection-search
         web-server/dispatch
         web-server/servlet/setup
         web-server/servlet/servlet-structs
         web-server/servlet-dispatch
         web-server/configuration/namespace
         web-server/private/mime-type
         (prefix-in lift: web-server/dispatchers/dispatch-lift)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         (prefix-in files: web-server/dispatchers/dispatch-files)
         (prefix-in servlets: web-server/dispatchers/dispatch-servlets)
         (prefix-in fsmap: web-server/dispatchers/filesystem-map))
(require "project-server-routes.rkt"
         "debug.rkt"
         "../setup.rkt"
         "../file.rkt"
         "../cache.rkt"
         "version.rkt")

(provide start-server)

(define (dispatcher-sequence . dispatchers)
  (let loop ([ds dispatchers] [r '()])
    (cond [(null? ds) (apply sequencer:make (reverse r))]
          [(not (car ds))   (loop (cdr ds) r)]
          [(list? (car ds)) (loop (append (car ds) (cdr ds)) r)]
          [else (loop (cdr ds) (cons (car ds) r))])))

(define default-web-root
  (collection-search '(lib "web-server/default-web-root")))

(define (start-server servlet-path [open-browser-window? #f])
  (define-values (pollen-servlet _)
    (dispatch-rules
     [((string-arg) ... (? (λ (x) (equal? "" x)))) route-index] ; last element of a "/"-terminated url is ""
     [((string-arg) ... (? pagetree-source?)) route-dashboard]
     [((string-arg) ... "in" (string-arg) ...) route-in]
     [((string-arg) ... "out" (string-arg) ...) route-out]
     [else route-default]))

  (message (format "Welcome to Pollen ~a" pollen:version) (format "(Racket ~a)" (version)))
  (message (format "Project root is ~a" (current-project-root)))

  (define server-name (format "http://localhost:~a" (current-server-port)))
  (message (format "Project server is ~a" server-name) "(Ctrl+C to exit)")
  (message (format "Project dashboard is ~a/~a" server-name (setup:main-pagetree)))
  (message "Ready to rock")

  ; modified from servlet-env.rkt in web-server

  (define server-root-path default-web-root)
  (define servlets-root (build-path server-root-path "htdocs"))
  (define mime-types-path (let ([p (build-path server-root-path "mime.types")])
                            (if (file-exists? p)
                                p
                                (build-path default-web-root "mime.types"))))
  (define (dispatcher sema)
    (dispatcher-sequence
     (dispatch/servlet pollen-servlet)

     (let-values ([(clear-cache! url->servlet)
                   (servlets:make-cached-url->servlet
                    (fsmap:filter-url->path
                     #rx"\\.(ss|scm|rkt|rktd)$"
                     (fsmap:make-url->valid-path
                      (fsmap:make-url->path servlets-root)))
                    (make-default-path->servlet
                     #:make-servlet-namespace
                     (make-make-servlet-namespace #:to-be-copied-module-specs empty)))])
       (servlets:make url->servlet))

     (files:make
      #:url->path (fsmap:make-url->path (current-server-extras-path))
      #:path->mime-type (make-path->mime-type mime-types-path)
      #:indices (list "index.html" "index.htm"))

     (files:make
      #:url->path (fsmap:make-url->path (current-project-root))
      #:path->mime-type (lambda (path)
                          (match (path-get-extension path)
                            [#".txt" #"text/plain; charset=utf-8"]
                            [_ ((make-path->mime-type mime-types-path) path)]))
      #:indices (list "index.html" "index.htm"))

     (files:make
      #:url->path (fsmap:make-url->path (build-path server-root-path "htdocs"))
      #:path->mime-type (make-path->mime-type mime-types-path)
      #:indices (list "index.html" "index.htm"))

     (lift:make (compose any->response route-404))))

  (parameterize ([error-print-width 1000])
    (serve/launch/wait
     dispatcher
     #:launch-path (and open-browser-window? servlet-path)
     #:listen-ip (current-server-listen-ip)
     #:port (current-server-port)))
  )
