#lang racket/base
(require pollen/main-base)

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [new-module-begin #%module-begin]))

(make-new-module-begin world:mode-markdown)