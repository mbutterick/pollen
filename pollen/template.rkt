#lang racket/base
(require sugar/test
         "template/base.rkt"
         "template/html.rkt")
(provide (all-from-out "template/base.rkt"
                       "template/html.rkt"))

(module-test-external
 (check-equal? (select* 'key  '#hash((key . "value"))) '("value"))
 (check-equal? (select 'key  '#hash((key . "value"))) "value")
 
 (define tx '(root (p "hello")))
 (check-equal? (->html tx) "<root><p>hello</p></root>"))