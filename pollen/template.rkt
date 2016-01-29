#lang racket/base
(require sugar/test
         "template/base.rkt"
         "template/html.rkt")
(provide (all-from-out "template/base.rkt"
                       "template/html.rkt"))