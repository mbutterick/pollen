#lang scribble/manual

@(require scribble/eval pollen/decode pollen/setup txexpr (for-label pollen/unstable/typography txexpr racket (except-in pollen #%module-begin)))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/unstable/typography))
@(require "mb-tools.rkt")


@title{Typography}

@defmodule[pollen/unstable/typography]

Quick & dirty utilities. I use them, but I haven't tested them with enough edge cases to feel like they deserve to live outside @racket[unstable]. I welcome improvements.


@defproc[
(smart-quotes
[xexpr (or/c string? txexpr?)]
[#:apostophe apostrophe-str string? "’"]
[#:single-open single-open-str string? "‘"]
[#:single-close single-close-str string? "’"]
[#:double-open double-open-str string? "“"]
[#:double-close double-close-str string? "”"])
(or/c string? txexpr?)]{

Convert straight quotes in @racket[xexpr] to curly. By default, American English curly quotes are used. The optional keyword arguments can be used to set different quotes suited to other languages or script systems. 

@examples[#:eval my-eval
(define tricky-string 
"\"Why,\" she could've asked, \"are we in O‘ahu watching 'Mame'?\"")
(display tricky-string)
(display (smart-quotes tricky-string))
(display (smart-quotes tricky-string
                      #:double-open "«" #:double-close "»"
                      #:single-open "‹" #:single-close "›"))
(display (smart-quotes tricky-string
                      #:double-open "„" #:double-close "”"
                      #:single-open "‚" #:single-close "’"))
]
}

@defproc[
(smart-dashes
[str string?])
string?]
In @racket[_str], convert three hyphens to an em dash, and two hyphens to an en dash, and remove surrounding spaces.

@examples[#:eval my-eval
(define tricky-string "I had a few --- OK, like 6--8 --- thin mints.")
(display tricky-string)
(display (smart-dashes tricky-string))
(code:comment @#,t{Monospaced font not great for showing dashes, but you get the idea})
]


@defproc[
(smart-ellipses
[str string?])
string?]
In @racket[_str], convert three periods to an ellipsis.

@examples[#:eval my-eval
(define tricky-string "I had a few ... OK, like 6--8 ... thin mints.")
(display tricky-string)
(display (smart-ellipses tricky-string))
]



@defproc[
(wrap-hanging-quotes
[tx txexpr?]
[#:single-preprend single-preprender txexpr-tag? 'squo]
[#:double-preprend double-preprender txexpr-tag? 'dquo]
)
txexpr?]
Find single or double quote marks at the beginning of @racket[_tx] and wrap them in an X-expression with the tag @racket[_single-preprender] or @racket[_double-preprender], respectively. The default values are @racket['squo] and @racket['dquo].

@examples[#:eval my-eval
(wrap-hanging-quotes '(p "No quote to hang."))
(wrap-hanging-quotes '(p "“What? We need to hang quotes?”"))
]

In pro typography, quotation marks at the beginning of a line or paragraph are often shifted into the margin slightly to make them appear more optically aligned with the left edge of the text. With a reflowable layout model like HTML, you don't know where your line breaks will be. 

This function will simply insert the @racket['squo] and @racket['dquo] tags, which provide hooks that let you do the actual hanging via CSS, like so (actual measurement can be refined to taste):

@verbatim{squo {margin-left: -0.25em;}
dquo {margin-left: -0.50em;}
}

Be warned: there are many edge cases this function does not handle well.

@examples[#:eval my-eval
(code:comment @#,t{Argh: this edge case is not handled properly})
(wrap-hanging-quotes '(p "“" (em "What?") "We need to hang quotes?”"))
]

@defproc[
(whitespace?
[v any/c])
boolean?]
A predicate that returns @racket[#t] for any stringlike @racket[_v] that's entirely whitespace, but also the empty string, as well as lists and vectors that are made only of @racket[whitespace?] members. Following the @racket[regexp-match] convention, @racket[whitespace?] does not return @racket[#t] for a nonbreaking space. If you prefer that behavior, use @racket[whitespace/nbsp?]. 


@examples[#:eval my-eval
(whitespace? "\n\n   ")
(whitespace? (string->symbol "\n\n   "))
(whitespace? "")
(whitespace? '("" "  " "\n\n\n" " \n"))
(define nonbreaking-space (format "~a" #\u00A0))
(whitespace? nonbreaking-space)
]

@defproc[
(whitespace/nbsp?
[v any/c])
boolean?]
Like @racket[whitespace?], but also returns @racket[#t] for nonbreaking spaces.


@examples[#:eval my-eval
(whitespace/nbsp? "\n\n   ")
(whitespace/nbsp? (string->symbol "\n\n   "))
(whitespace/nbsp? "")
(whitespace/nbsp? '("" "  " "\n\n\n" " \n"))
(define nonbreaking-space (format "~a" #\u00A0))
(whitespace/nbsp? nonbreaking-space)
]