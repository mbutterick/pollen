#lang scribble/manual

@(require scribble/eval pollen/render pollen/world (for-label racket (except-in pollen #%module-begin) pollen/world sugar))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/file))


@title{Installation}

@link["http://download.racket-lang.org/"]{Install Racket}, which includes DrRacket.

From the command line, install Pollen:
@verbatim{raco pkg install pollen}

After that, you can update the package from the command line:
@verbatim{raco pkg update pollen}


Windows users, I'll trust you to convert @tt{raco} into the appropriate command for your system — assuming defaults, it's likely to be @tt{"C:\Program Files\Racket\raco"} (include the surrounding quotes in the command).
