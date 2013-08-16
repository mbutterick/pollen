#lang racket

(define-syntax (handle-pollen-command stx)
  (datum->syntax stx
                 (let ([arg (if (= (vector-length (current-command-line-arguments)) 0)
                                ""
                                (vector-ref (current-command-line-arguments) 0))])
                   (case arg
                     [("serve") 
                      `(require "server.rkt")]
                     [("regenerate")  
                      `(begin
                         (displayln "Regenerate all...")
                         (require "regenerate.rkt")
                         (regenerate-all-files))]
                     [("clone")
                      (let ([target-path (if (> (vector-length (current-command-line-arguments)) 1)
                                             (string->path (vector-ref (current-command-line-arguments) 1))
                                             (build-path (find-system-path 'desk-dir) (string->path "clone")))])
                        
                        `(begin
                           (displayln "Clone & bone...")
                           (require racket/file)
                           (require "tools.rkt")
                           
                           (define (pollen-related-file? file)
                             (any (list
                                        pollen-source? 
                                        preproc-source? 
                                        template-source?
                                        pmap-source?
                                        pollen-script?
                                        magic-directory?
                                        racket-file?)
                                  file))
                           
                           (define (delete-it path)
                             (when (directory-exists? path)
                               (delete-directory/files path))
                             (when (file-exists? path)
                               (delete-file path)))
                           
                           (let ([source-dir (current-directory)]
                                 [target-dir ,target-path])
                             (when (directory-exists? target-dir)
                               (delete-directory/files target-dir))
                             (copy-directory/files source-dir target-dir)
                             (map delete-it (find-files pollen-related-file? target-dir))
                             (displayln (format "Completed to ~a" ,target-path))
                             )))]
                     [("")
                      `(displayln "No command given")]
                     [else
                      (let ([possible-file (string->path arg)])
                        (if (file-exists? possible-file)
                            `(begin
                               (require (planet mb/pollen/regenerate))
                               (regenerate ,possible-file))
                            `(displayln (format "No command defined for ~a" ,arg))))]))))

(handle-pollen-command)
