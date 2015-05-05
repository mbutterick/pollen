#lang scribble/manual

@(require scribble/eval pollen/render pollen/world (for-label racket (except-in pollen #%module-begin) pollen/world sugar))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/file))


@title{Installation}

@link["http://download.racket-lang.org/"]{Install Racket}, which includes DrRacket.

Linux and Mac users: update your @envvar{PATH} to include @filepath{/path-to-new-racket-directory/bin/}. Then you'll have access to @exec{raco} (see @other-doc['(lib "scribblings/raco/raco.scrbl")]).

Mac users who haven't done this before: @link["http://architectryan.com/2012/10/02/add-to-the-path-on-mac-os-x-mountain-lion/"]{these instructions} are simple and accurate.

Windows users, I'll trust you to convert @exec{raco} into the appropriate command for your system — assuming defaults, it's likely to be @filepath{C:\Program Files\Racket\raco} (include the surrounding quotes in the command).

Then, from the command line, install Pollen:
@commandline{raco pkg install pollen}

After that, you can update the package from the command line:
@commandline{raco pkg update pollen}