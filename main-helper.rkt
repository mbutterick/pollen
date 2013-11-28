#lang racket
(require racket/contract/region)

;; use full planet paths because this file is evaluated from source directory,
;; not module directory
(require (for-syntax racket/rerequire (planet mb/pollen/tools) (planet mb/pollen/world)))
(require (planet mb/pollen/tools) (planet mb/pollen/world))

(provide (all-defined-out))

(module+ test (require rackunit))

(define-for-syntax (put-file-in-require-form file)
  `(file ,(->string file)))

(define-syntax (require-and-provide-extras stx)
  (define project-require-files (get-project-require-files))
  (if project-require-files
     (let ([files-in-require-form (map put-file-in-require-form project-require-files)])
       (datum->syntax stx `(begin
                             (require ,@files-in-require-form)
                             (provide (all-from-out ,@files-in-require-form)))))
    ; if no files to import, do nothing
    #'(begin)))

(define-syntax (require-extras stx)
  (define project-require-files (get-project-require-files))
  (if project-require-files
     (let ([files-in-require-form (map put-file-in-require-form project-require-files)])
       (datum->syntax stx `(begin
                             (require ,@files-in-require-form))))
    ; if no files to import, do nothing
    #'(begin)))


;; here = path of this file, relative to current directory. 
;; We want to make this identifier behave as a runtime function
;; This requires two steps.
;; First, define the underlying function as syntax-rule
(define-syntax (get-here-path stx)
  (datum->syntax stx 
                 '(begin 
                    ;; Even though begin permits defines,
                    ;; This macro might be used in an expression context, 
                    ;; whereupon define would cause an error.
                    ;; Therefore, best to use let.
                    (let* ([ccr (current-contract-region)] ; trick for getting current module name
                           [hp (cond
                                        ;; if contract-region is called from within submodule,
                                        ;; you get a list
                                        ;; in which case, just grab the path from the front
                                        [(list? ccr) (car ccr)]
                                        ;; file isn't yet saved in drracket
                                        [(equal? 'pollen-lang-module ccr) 'nowhere] 
                                        [else ccr])])
                      ;; pass complete path back as here value (as string)
                      ;; Why not relative to current-directory? 
                      ;; Because current-directory can't be parameterized
                      ;; so raises possibility of inconsistent values.
                      ;; Whereas the complete path is unambiguous,
                      ;; and can be made relative by the caller (or otherwise altered).
                      (->string hp)))))

;; todo: update tests
;(module+ test
;  (check-equal? (get-here) "main-helper.rkt"))

; Second step: apply a separate syntax transform to the identifier itself
; We can't do this in one step, because if the macro goes from identifier to function definition,
; The macro processor will evaluate the body at compile-time, not at runtime.
(define-syntax here-path (λ(stx) (datum->syntax stx '(get-here-path))))

;; todo: update test
;(module+ test
;  (check-equal? here "main-helper.rkt"))


