#lang scribble/manual

@(require scribble/eval pollen/decode pollen/setup txexpr (for-label pollen/unstable/pygments txexpr racket (except-in pollen #%module-begin)))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/unstable/pygments))
@(require "mb-tools.rkt")


@title{Pygments}

@defmodule[pollen/unstable/pygments]

A simple interface to syntax highlighting using Pygments. @bold{You must already have Pygments installed to use this module.} See the mini-tutorial @seclink["pygments-with-pollen"].

@defproc[
(highlight
[language symbol?]
[#:python-executable python-executable path-string? "python"]
[#:line-numbers? line-numbers? boolean? #t]
[#:css-class css-class string? "source"]
[#:lines lines (listof number?) '()]
[lines-of-code string?] ...
)
txexpr?]
Use Pygments to highlight the code in @racket[_lines-of-code] according to the semantics of @racket[_language]. Does not apply any visual styling, just the markup. The optional @racket[_python-executable] controls which Python Pollen should use (which will be useful when you install Pygments in a non-default version of Python). The optional @racket[_line-numbers?] controls whether or not the syntax highlight should include line numbers. The optional @racket[_css-class] controls the CSS class name for the syntax highlight. The optional @racket[_lines] will tag the specified lines of code with the CSS class @tt{hll}.

Sample input:

@codeblock{
#lang pollen
◊(require pollen/unstable/pygments)
◊highlight['python]{
for x in range(3):
    print x
}
}

Output from this sample:

@repl-output{
'(div ((class "highlight")) (table ((class "sourcetable")) (tbody () (tr () (td ((class "linenos")) (div ((class "linenodiv")) (pre () "1\n2"))) (td ((class "code")) (div ((class "source")) (pre () (span ((class "k")) "for") " " (span ((class "n")) "x") " " (span ((class "ow")) "in") " " (span ((class "nb")) "range") (span ((class "p")) "(") (span ((class "mi")) "3") (span ((class "p")) "):") "\n    " (span ((class "k")) "print") " " (span ((class "n")) "x") "\n")) "\n")))) "\n")
}