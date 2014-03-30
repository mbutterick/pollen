#lang racket/base
(require rackunit racket/path pollen/cache pollen/file)
(require "../template.rkt")


(check-equal? (put '(foo "bar")) '(foo "bar"))
(check-equal? (put "tests/template/put.pd") 
              '(root "\n" "\n" (em "One") " paragraph" "\n" "\n" "Another " (em "paragraph") "\n" "\n"))


(module+ test
  (check-false (puttable-item? #t))
  (check-false (puttable-item? #f)))




(parameterize ([current-directory "tests/template"])
  (check-false (find "nonexistent-key" "put"))
  (check-equal? (find "foo" "put") "bar")
  (check-equal? (find "em" "put") "One"))
(check-equal? (find "foo" #f) #f)

(parameterize ([current-directory "tests/template"])
  (check-equal? (find-in-metas "put" "foo") (list "bar"))
  (let* ([metas (cached-require (->markup-source-path 'put) 'metas)]
         [here (find-in-metas 'put 'here)])     
    (check-equal? here (list "tests/template/put"))))


(parameterize ([current-directory "tests/template"])
  (check-false (find-in-doc "put" "nonexistent-key"))
  (check-equal? (find-in-doc "put" "em") (list "One" "paragraph")))

(check-equal? (splice '(p "foo" "bar")) (list "foo" "bar"))
(check-equal? (splice (list "foo" "bar")) (list "foo" "bar"))
(check-equal? (splice "foo") (list "foo"))