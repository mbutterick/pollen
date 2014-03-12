#lang scribble/manual

@(require scribble/eval pollen/cache pollen/world (for-label racket (except-in pollen #%module-begin) pollen/world pollen/ptree txexpr))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/ptree))

@section{Ptrees}

@defmodule[pollen/ptree]

A @italic{ptree} — short for @italic{Pollen tree} — is a hierchical list of Pollen output files. A ptree source file has the extension @code[(format ".~a" world:ptree-source-ext)]. A ptree provides a convenient way of separating the structure of the pages from the page sources, and navigating around this structure.

Books and other long documents are usually organized in a structured format — at minimum they have a sequence of pages, but more often they have hierarchical sections with subsequences within. Individual Pollen source files don't know anything about how they're connected to other files. (Well, you could maintain this information within each source file, but this would be a poor use of human energy.) 

@defproc[
(ptree?
[v any/c])
boolean?]
Test whether @racket[_v] is a valid ptree. A valid ptree is a @racket[txexpr?] whose elements are either @racket[pnode?] or @racket[ptree?]. Also, all the pnodes in a ptree must be unique.

@examples[#:eval my-eval
(ptree? '(index.html))
(ptree? '(index.html index.html))
(define nested-pt '(1.html 2.html (3.html 3a.html 3b.html)))
(ptree? nested-pt)
(ptree? `(index.html ,nested-pt (subsection.html more.html)))
(ptree? `(index.html ,nested-pt (subsection.html more.html) ,nested-pt))
]
