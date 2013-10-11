#! /Applications/Racket/bin/racket
#lang web-server
(require web-server/servlet-env)
(require web-server/dispatch web-server/dispatchers/dispatch)
(require xml)
(require "server-routes.rkt" "predicates.rkt")

(displayln "Pollen server starting..." (current-error-port))

(define/contract (route-wrapper route-proc)
  ;; todo: make better contract for return value
  (procedure? . -> . procedure?)
  (λ(req string-arg) 
    (define filename string-arg)
    (response/xexpr (route-proc (build-path pollen-file-root filename)))))

(define-values (start url)
  (dispatch-rules
   [("start") (λ(req) (response/xexpr (route-index pollen-file-root)))]
   [("source" (string-arg)) (route-wrapper route-source)]
   [("xexpr" (string-arg)) (route-wrapper route-xexpr)]
   [("raw" (string-arg)) (route-wrapper route-raw-html)]
   [("html" (string-arg)) (route-wrapper route-html)]
   [else  (λ(req)
            ;; because it's the "else" route, can't use string-arg matcher
            (define request-url (request-uri req))
            ;; /inform is a magic request that must be allowed to pass through
            (when (not (equal? (url->string request-url) "/inform"))
              (let ([path (reroot-path (url->path request-url) pollen-file-root)]
                    [force (equal? (get-query-value request-url 'force) "true")])
                (route-default path #:force force)))
            (next-dispatcher))]))

(displayln "Ready to rock" (current-error-port))

(serve/servlet start
               #:port 8080
               #:listen-ip #f
               #:servlet-regexp #rx"" ; respond to top level
               #:command-line? #t
               #:extra-files-paths (list (build-path pollen-file-root)))