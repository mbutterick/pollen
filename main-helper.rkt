#lang racket
(require racket/contract/region)

;; use full planet paths because this file is evaluated from source directory,
;; not module directory
(require (for-syntax (planet mb/pollen/tools) (planet mb/pollen/world)))
(require (planet mb/pollen/tools) (planet mb/pollen/world))

(provide (all-defined-out))

(module+ test (require rackunit))

;; Look for an EXTRAS_DIR directory local to the source file.
;; and require all the .rkt files therein. 
;; optionally provide them.
(define-syntax (require-extras stx #:provide [provide #f])  
  (cond
    [(directory-exists? EXTRAS_DIR)
     ;; This will be resolved in the context of current-directory.
     ;; So when called from outside the project directory, 
     ;; current-directory must be properly set with 'parameterize'
     (define (make-complete-path path)
       ;; todo: document why this function is necessary (it definitely is, but I forgot why)
       (define-values (start_dir name _ignore) (split-path (path->complete-path path)))
       (build-path start_dir EXTRAS_DIR name))
     (define files (map make-complete-path (filter (λ(i) (has-ext? i 'rkt)) (directory-list EXTRAS_DIR))))
     (define files-in-require-form 
       (map (λ(file) `(file ,(->string file))) files)) 
     (datum->syntax stx 
                    (if provide
                        `(begin
                           (require ,@files-in-require-form)
                           (provide (all-from-out ,@files-in-require-form)))
                        `(begin
                           (require ,@files-in-require-form))))]
    ; if no files to import, do nothing
    [else #'(begin)]))


;; here = path of this file, relative to current directory. 
;; We want to make this identifier behave as a runtime function
;; This requires two steps.
;; First, define the underlying function as syntax-rule
(define-syntax (get-here stx)
  (datum->syntax stx 
                 '(begin 
                    ;; Even though begin permits defines,
                    ;; This macro might be used in an expression context, 
                    ;; whereupon define would cause an error.
                    ;; Therefore, best to use let.
                    (let* ([ccr (current-contract-region)] ; trick for getting current module name
                           [here-path (cond
                                        ;; if contract-region is called from within submodule,
                                        ;; you get a list
                                        ;; in which case, just grab the path from the front
                                        [(list? ccr) (car ccr)]
                                        ;; file isn't yet saved in drracket
                                        [(equal? 'pollen-lang-module ccr) 'nowhere] 
                                        [else ccr])])
                      (displayln "ccr in main=")
                      (displayln ccr)
                      (displayln "current-directory in main=")
                      ;; todo: this does not respond to parameterize. Why?
                      (displayln (current-directory))
                      (->string here-path)))))

(module+ test
  (check-equal? (get-here) "main-helper.rkt"))

; Second step: apply a separate syntax transform to the identifier itself
; We can't do this in one step, because if the macro goes from identifier to function definition,
; The macro processor will evaluate the body at compile-time, not at runtime.
(define-syntax here (λ(stx) (datum->syntax stx '(get-here))))

(module+ test
  (check-equal? here "main-helper.rkt"))


