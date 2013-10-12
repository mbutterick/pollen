#lang web-server
(require web-server/servlet-env)
(require web-server/dispatch web-server/dispatchers/dispatch)
(require xml)
(require "server-routes.rkt" "predicates.rkt" "debug.rkt")

(message "Pollen server starting...")
(message "Racket version" (version))

(define (logger req)
  (message (url->string (request-uri req)) "from" (request-client-ip req)))

(define/contract (route-wrapper route-proc)
  (procedure? . -> . procedure?)
  (λ(req string-arg) 
    (logger req)
    (define filename string-arg) 
    (response/xexpr (route-proc (build-path pollen-file-root filename)))))

(define-values (start url)
  (dispatch-rules
   [("start") (λ(req) 
                (logger req)
                (response/xexpr (route-index)))]
   [("source" (string-arg)) (route-wrapper route-source)]
   [("xexpr" (string-arg)) (route-wrapper route-xexpr)]
   [("raw" (string-arg)) (route-wrapper route-raw-html)]
   [("html" (string-arg)) (route-wrapper route-html)]
   [else  (λ(req)
            ;; because it's the "else" route, can't use string-arg matcher
            (logger req)
            (route-default req)
            (next-dispatcher))]))

(message "Ready to rock")

(serve/servlet start
               #:port 8088
               #:listen-ip #f
               #:servlet-regexp #rx"" ; respond to top level
               #:command-line? #t
               #:extra-files-paths (list (build-path pollen-file-root)))