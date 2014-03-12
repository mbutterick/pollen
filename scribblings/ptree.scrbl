#lang scribble/manual

@(require scribble/eval pollen/cache pollen/world (for-label racket (except-in pollen #%module-begin) pollen/world pollen/ptree txexpr sugar pollen/decode xml))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/ptree))

@section{Ptrees}

@defmodule[pollen/ptree]

A @italic{ptree} — short for @italic{Pollen tree} — is a hierarchical list of Pollen output files. A ptree source file has the extension @code[(format ".~a" world:ptree-source-ext)]. A ptree provides a convenient way of separating the structure of the pages from the page sources, and navigating around this structure.

Books and other long documents are usually organized in a structured way — at minimum they have a sequence of pages, but more often they have sections with subsequences within. Individual Pollen source files don't know anything about how they're connected to other files. In theory, you could maintain this information within each source file. This would be a poor use of human energy. Let the ptree figure it out.

@defproc[
(ptree?
[v any/c])
boolean?]
Test whether @racket[_v] is a valid ptree: a @racket[txexpr?] whose elements are either @racket[pnode?] or @racket[ptree?]. Also, all the pnodes in a ptree must be unique. The root node is ignored.

@examples[#:eval my-eval
(ptree? '(root index.html))
(ptree? '(root index.html index.html))
(define nested-pt '(root 1.html 2.html (3.html 3a.html 3b.html)))
(ptree? nested-pt)
(ptree? `(root index.html ,nested-pt (subsection.html more.html)))
(ptree? `(root index.html ,nested-pt (subsection.html ,nested-pt)))
]

@defproc[
(pnode?
[v any/c])
boolean?]
Test whether @racket[_v] is a valid pnode. Every leaf of a ptree is a pnode. A pnode can be any @racket[stringish?] value that is both an @racket[xexpr?] and not @racket[whitespace/nbsp?] In practice, your pnodes will likely be names of output files.

@examples[#:eval my-eval
(map pnode? (list 'symbol "string" "index.html" "\n\n ah! \n\n"))
(map pnode? (list 9.999 (string->path "index.html") '(p "Hello") "\n\n"))
]



@defparam[current-ptree ptree ptree?
          #:value #f]{
A parameter that defines the default ptree used by ptree navigation functions if another is not explicitly specified.}

@defparam[current-url-context dir path?
          #:value world:current-project-root]{
A parameter that defines the default directory used to resolve @racket[pnode->url]. Initialized to the root directory of the current project.}
