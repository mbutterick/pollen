#lang scribble/manual

@(require scribble/eval pollen/decode pollen/world (prefix-in html: pollen/html) txexpr (for-label txexpr racket (except-in pollen #%module-begin)))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/pygments))
@(require "mb-tools.rkt")


@title{Pygments}

@defmodule[pollen/pygments]

A simple interface to syntax highlighting using Pygments. @bold{You must already have Pygments installed to use this module.} See the mini-tutorial @seclink["pygments-with-pollen"].

@defproc[
(highlight
[language symbol?]
[lines-of-code string?] ...
)
txexpr?]
Use Pygments to highlight the code in @racket[_lines-of-code] according to the semantics of @racket[_language]. Does not apply any visual styling, just the markup.

Sample input:

@codeblock{
#lang pollen
◊(require pollen/pygments)
◊highlight['python]{
for x in range(3):
    print x
}
}

Output from this sample:

@repl-output{
'(div ((class "highlight")) (table ((class "sourcetable")) (tbody () (tr () (td ((class "linenos")) (div ((class "linenodiv")) (pre () "1\n2"))) (td ((class "code")) (div ((class "source")) (pre () (span ((class "k")) "for") " " (span ((class "n")) "x") " " (span ((class "ow")) "in") " " (span ((class "nb")) "range") (span ((class "p")) "(") (span ((class "mi")) "3") (span ((class "p")) "):") "\n    " (span ((class "k")) "print") " " (span ((class "n")) "x") "\n")) "\n")))) "\n")
}