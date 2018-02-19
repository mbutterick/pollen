#lang pollen
◊(require rackunit)
◊(define-meta foo "bar")
◊(check-equal? ◊(test-current-metas) "foo here-path zim")
◊(define-meta zim "zam")
