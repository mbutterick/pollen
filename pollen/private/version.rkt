#lang racket/base
(require racket/file racket/format racket/runtime-path)
(provide pollen:version pollen:version-strict)
(define-runtime-path ts-file "ts.rktd")

(define major-version 1)
(define minor-version 3)

(define pollen:version-strict (format "~a.~a" major-version minor-version))

(define (generate-full-version)
  (define pollen-birthday 1375340400) ; 8/1/2013, 12:00:00 AM GMT-7:00 DST
  (define seconds-per-year (* 60 60 24))
  (define build-time (- (file->value ts-file) pollen-birthday))
  (define-values (build-days seconds-remainder) (quotient/remainder build-time seconds-per-year))
  (define minutes-remainder (quotient seconds-remainder 60))
  (format "~a.~a.~a" pollen:version-strict build-days minutes-remainder))

(define pollen:version (generate-full-version))

