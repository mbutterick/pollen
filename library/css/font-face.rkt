#lang racket/base
(require "core.rkt")
(require pollen/file-tools)
(require net/url-structs net/base64 racket/file)
(provide (all-defined-out))

(module+ test (require rackunit))


(define/contract (base64-font-string? x)
  (any/c . -> . boolean?)
  ((->string x) . starts-with? . "data:"))

(module+ test
  (check-true (base64-font-string? "data:foobar"))
  (check-false (base64-font-string? "foobar")))


(define/contract (font-format p)
  (pathish? . -> . (or/c string? #f))
  (case (get-ext (->path p))
    [("eot") "embedded-opentype"]
    [("woff") "woff"]
    [("ttf" "otf") "truetype"] ; yep, in this CSS declaration, otf is considered 'truetype'
    [("svg") "svg"]
    [else #f]))

(module+ test
  (check-equal? (font-format "foo.eot") "embedded-opentype")
  (check-equal? (font-format "foo.woff") "woff")
  (check-equal? (font-format "foo.ttf") "truetype")
  (check-equal? (font-format "foo.otf") "truetype")
  (check-equal? (font-format "foo.svg") "svg")
  (check-false (font-format "foo")))


(define/contract (font-mime-type p)
  (pathish? . -> . (or/c string? #f))
  (case (get-ext (->path p))
    [("eot") "application/vnd.ms-fontobject"]
    [("woff") "application/font-woff"]
    [("ttf") "application/x-font-truetype"]
    [("otf") "application/x-font-opentype"]
    [("svg") "image/svg+xml"]
    [else #f]))

(module+ test
  (check-equal? (font-mime-type "foo.eot") "application/vnd.ms-fontobject")
  (check-equal? (font-mime-type (->url "foo.woff?bar=ino")) "application/font-woff")
  (check-equal? (font-mime-type "foo.ttf") "application/x-font-truetype")
  (check-equal? (font-mime-type "foo.otf") "application/x-font-opentype")
  (check-equal? (font-mime-type "foo.svg") "image/svg+xml")
  (check-false (font-mime-type "foo")))


(define/contract (path->base64-font-string p)
  (pathish? . -> . base64-font-string?)
  (define path (->path p))
  ;; for CSS, base64 encode needs to be done with no line separator
  (format "data:~a;charset=utf-8;base64,~a" (font-mime-type p) (base64-encode (file->bytes path) #"")))




(define/contract (font-face-declaration font-family 
                                        src-url
                                        #:font-style [font-style "normal"] 
                                        #:font-weight [font-weight "normal"]
                                        #:font-stretch [font-stretch "normal"]
                                        #:base64 [base64? #f])
  ((string? (or/c urlish? base64-font-string?)) 
   (#:font-style string?  #:font-weight string? #:font-stretch string? #:base64 boolean?)
   . ->* . string?)
  (let* [(url (->url src-url))
         (url-value (if base64? (path->base64-font-string src-url) (->path url)))
         (src (format "url('~a') format('~a')" url-value (font-format src-url)))]
    (string-append "@font-face {\n" 
                   (join-css-strings (map make-css-string 
                                          '(font-family font-style font-weight font-stretch src)
                                          (list font-family font-style font-weight font-stretch src)))
                   "}")))

(define ffd font-face-declaration)

(module+ main
(display (ffd "Miso" "charter-regular.woff" #:font-style "italic" #:font-weight "700" #:base64 #t)))