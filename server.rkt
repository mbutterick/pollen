#lang web-server
(require "startup.rkt")
(require web-server/servlet-env)
(require web-server/dispatch web-server/dispatchers/dispatch)
(require xml)
(require "server-routes.rkt" "debug.rkt" "readability.rkt" "world.rkt")

(define port-number 8088)

(message (format "Project root is ~a" PROJECT_ROOT))
(message (format "Project server is http://localhost:~a" port-number) "(Ctrl-C to exit)")

(define (logger req)
  (define client (request-client-ip req))
  (define url-string (url->string (request-uri req)))
  (message "Request:" (string-replace url-string DASHBOARD_NAME " dashboard")
           "from" (if (equal? client "::1") "localhost" client)))

(define/contract (route-wrapper route-proc)
  (procedure? . -> . procedure?)
  (λ(req string-arg) 
    (logger req)
    (define filename string-arg) 
    (response/xexpr (route-proc (build-path PROJECT_ROOT filename)))))

(define-values (start url)
  (dispatch-rules
   ;; the match patterns for each rule represent /each/slashed/piece of a url
   ;; (as if the url is split on slashes into a list before matching)
   ;; dashboard page: works on any url of form /dir/dir/dir/poldash.html
   ;; todo: figure out how to use world:DASHBOARD_NAME here
   [((string-arg) ... "poldash.html") (λ(req . string-args) 
                                        (logger req) 
                                        (define subdirs (flatten string-args))
                                        (define dir (apply build-path PROJECT_ROOT subdirs))
                                        (response/xexpr (route-dashboard dir)))]
   ;; raw viewer: works on any url of form /dir/dir/raw/name.html 
   ;; (pattern matcher automatically takes out the "raw")
   [((string-arg) ... "raw" (string-arg)) (λ(req . string-args) 
                                            (logger req) 
                                            (define path (apply build-path PROJECT_ROOT (flatten string-args)))

                                            (response/xexpr (route-raw path)))]
   [("xexpr" (string-arg)) (route-wrapper route-xexpr)]
   [("html" (string-arg)) (route-wrapper route-html)]
   [else  (λ(req)
            ;; because it's the "else" route, can't use string-arg matcher
            (logger req)
            (route-default req)
            (next-dispatcher))]))

(message (format "Project dashboard is http://localhost:~a/pollen.html" port-number))
(message "Ready to rock")


(serve/servlet start
               #:port port-number
               #:listen-ip #f
               #:servlet-regexp #rx"" ; respond to top level
               #:command-line? #t
               #:extra-files-paths (list (build-path PROJECT_ROOT)))