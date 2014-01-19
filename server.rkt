#lang web-server
(require web-server/servlet-env 
         web-server/dispatch)
(require "server-routes.rkt" 
         "debug.rkt" 
         "world.rkt")

(define-values (pollen-servlet url)
  (dispatch-rules
   ;; todo: figure out how to use world:DASHBOARD_NAME here
   [((string-arg) ... "poldash.html") route-dashboard]
   [((string-arg) ... "raw" (string-arg)) route-raw]
   [((string-arg) ... "xexpr" (string-arg)) route-xexpr]
   ;;  [((string-arg) ... "force" (string-arg)) (route-wrapper route-force)]
   [else route-default]))

(message (format "Welcome to Pollen ~a" POLLEN_VERSION) (format "(Racket ~a)" (version)))
(message (format "Project root is ~a" PROJECT_ROOT))

(define server-name (format "http://localhost:~a" SERVER_PORT))
(message (format "Project server is ~a" server-name) "(Ctrl-C to exit)")
(message (format "Project dashboard is ~a/pollen.html" server-name))

(message "Ready to rock")

(serve/servlet pollen-servlet
               #:port SERVER_PORT
               #:listen-ip #f
               #:servlet-regexp #rx"" ; respond to top level
               #:command-line? #t
               #:file-not-found-responder route-404
               #:extra-files-paths (list SERVER_EXTRAS_DIR PROJECT_ROOT))