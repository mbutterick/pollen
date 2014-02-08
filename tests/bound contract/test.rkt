#lang racket
(require "bound.rkt")

(bar "hello") ; bar is unbound
((bound/c bar) "hello")
