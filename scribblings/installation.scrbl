#lang scribble/manual

@(require scribble/eval pollen/render pollen/world (for-label racket (except-in pollen #%module-begin) pollen/world sugar))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/file))


@title{Installation}

Install Racket, which includes DrRacket.

Install Pollen from the command line:
@verbatim{raco pkg install pollen}

After that, you can update the package from the command line:
@verbatim{raco pkg update pollen}
