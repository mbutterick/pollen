#lang web-server/base

(require racket/list
         web-server/servlet-env 
         web-server/dispatch)
(require "project-server-routes.rkt" 
         "debug.rkt" 
         "../setup.rkt"
         "../file.rkt"
         "../cache.rkt"
         "version.rkt")

(provide start-server)

(define (start-server)
  (define-values (pollen-servlet _)
    (dispatch-rules
     [((string-arg) ... (? pagetree-source?)) route-dashboard]
     [("in" (string-arg) ...) route-in]
     [("out" (string-arg) ...) route-out]
     [else route-default]))
  
  (message (format "Welcome to Pollen ~a" pollen:version) (format "(Racket ~a)" (version)))
  (message (format "Project root is ~a" (current-project-root)))
  
  (define server-name (format "http://localhost:~a" (current-server-port)))
  (message (format "Project server is ~a" server-name) "(Ctrl+C to exit)")
  (message (format "Project dashboard is ~a/~a" server-name (setup:main-pagetree)))
  (message "Ready to rock")
    
  (parameterize ([error-print-width 1000])
    (serve/servlet pollen-servlet
                   #:port (current-server-port)
                   #:listen-ip #f
                   #:servlet-regexp #rx"" ; respond to top level
                   #:command-line? #t
                   #:file-not-found-responder route-404
                   #:extra-files-paths (list (current-server-extras-path) (current-project-root)))))