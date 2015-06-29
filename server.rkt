#lang web-server/base

(require racket/list
         web-server/servlet-env 
         web-server/dispatch)
(require "server-routes.rkt" 
         "debug.rkt" 
         "world.rkt"
         "file.rkt"
         "cache.rkt")

(provide start-server)

(define (start-server)
  (define-values (pollen-servlet _)
    (dispatch-rules
     [((string-arg) ... (? pagetree-source?)) route-dashboard]
     [((string-arg) ... "in" (string-arg)) route-in]
     [((string-arg) ... "out" (string-arg)) route-out]
     [((string-arg) ... "xexpr" (string-arg)) route-xexpr]
     [else route-default]))
  
  (message (format "Welcome to Pollen ~a" (world:current-pollen-version)) (format "(Racket ~a)" (version)))
  (message (format "Project root is ~a" (world:current-project-root)))
  
  (define server-name (format "http://localhost:~a" (world:current-server-port)))
  (message (format "Project server is ~a" server-name) "(Ctrl-C to exit)")
  (message (format "Project dashboard is ~a/~a" server-name (world:current-default-pagetree)))
  (message "Ready to rock")
    
  (parameterize ([error-print-width 1000]
                 [current-cache (make-cache)])
    (serve/servlet pollen-servlet
                   #:port (world:current-server-port)
                   #:listen-ip #f
                   #:servlet-regexp #rx"" ; respond to top level
                   #:command-line? #t
                   #:file-not-found-responder route-404
                   #:extra-files-paths (list (world:current-server-extras-path) (world:current-project-root)))))