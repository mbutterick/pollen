#! /Applications/Racket/bin/racket
#lang web-server
(require web-server/servlet-env)
(require web-server/dispatch web-server/dispatchers/dispatch)
(require "server-routes.rkt" "predicates.rkt")

(displayln "Pollen server starting...")

(define pollen-file-root (current-directory))

(define/contract (route-wrapper route-proc)
  ;; todo: make better contract for return value
  ((complete-path? . -> . tagged-xexpr?) . -> . procedure?)
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
            ;; so extract the path manually
            (define req-uri (request-uri req))
            (define path (reroot-path (url->path req-uri) pollen-file-root))
            (define force (get-query-value req-uri 'force))
            (route-preproc path #:force force)
            (next-dispatcher))]))

(displayln "Ready to rock")

(serve/servlet start
               #:port 8080
               #:listen-ip #f
               #:servlet-regexp #rx"" ; respond to top level
               #:command-line? #t
               #:extra-files-paths (list (build-path pollen-file-root)))