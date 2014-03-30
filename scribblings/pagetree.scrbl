#lang scribble/manual

@(require scribble/eval pollen/cache pollen/world (for-label racket (except-in pollen #%module-begin) pollen/world pollen/pagetree txexpr pollen/decode pollen/file))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/pagetree txexpr))

@title{Pagetree}

@defmodule[pollen/pagetree]

A @italic{pagetree} is a hierarchical list of Pollen output files. A pagetree source file has the extension @code[(format ".~a" world:pagetree-source-ext)]. A pagetree provides a convenient way of separating the structure of the pages from the page sources, and navigating around this structure.

Pagetrees are made of @italic{pagenodes}. Usually these pagenodes will be names of output files in your project. (If you think it would've been more logical to just call them ``pages,'' perhaps. When I think of a web page, I think of a file on a disk. Whereas pagenodes may — and often do — refer to files that don't yet exist.)

Books and other long documents are usually organized in a structured way — at minimum they have a sequence of pages, but more often they have sections with subsequences within. Individual Pollen source files don't know anything about how they're connected to other files. In theory, you could maintain this information within each source file. This would be a poor use of human energy. Let the pagetree figure it out.

@defproc[
(pagetree?
[possible-pagetree any/c])
boolean?]
Test whether @racket[_possible-pagetree] is a valid pagetree. It must be a @racket[txexpr?] where all elements are @racket[pagenode?], and each is unique within @racket[_possible-pagetree] (not counting the root node).

@examples[#:eval my-eval
(pagetree? '(root index.html))
(pagetree? '(root duplicate.html duplicate.html))
(pagetree? '(root index.html "string.html"))
(define nested-ptree '(root 1.html 2.html (3.html 3a.html 3b.html)))
(pagetree? nested-ptree)
(pagetree? `(root index.html ,nested-ptree (subsection.html more.html)))
(code:comment @#,t{Nesting a subtree twice creates duplication})
(pagetree? `(root index.html ,nested-ptree (subsection.html ,nested-ptree)))
]

@defproc[
(validate-pagetree
[possible-pagetree any/c])
pagetree?]
Like @racket[pagetree?], but raises a descriptive error if @racket[_possible-pagetree] is invalid, and otherwise returns @racket[_possible-pagetree] itself.

@examples[#:eval my-eval
(validate-pagetree '(root (mama.html son.html daughter.html) uncle.html))
(validate-pagetree `(root (,+ son.html daughter.html) uncle.html))
(validate-pagetree '(root (mama.html son.html son.html) mama.html))
]


@defproc[
(pagenode?
[possible-pagenode any/c])
boolean?]
Test whether @racket[_possible-pagenode] is a valid pagenode. A pagenode can be any @racket[symbol?] that is not @racket[whitespace/nbsp?] Every leaf of a pagetree is a pagenode. In practice, your pagenodes will likely be names of output files. 

@margin-note{Pagenodes are symbols (rather than strings) so that pagetrees will be valid tagged X-expressions, which is a more convenient format for validation & processing.}

@examples[#:eval my-eval
(code:comment @#,t{Three symbols, the third one annoying but valid})
(map pagenode? '(symbol index.html |   silly   |))
(code:comment @#,t{A number, a string, a txexpr, and a whitespace symbol})
(map pagenode? '(9.999 "index.html" (p "Hello") |    |))
]


@defproc[
(pagenodeish?
[v any/c])
boolean?]
Return @racket[#t] if @racket[_v] can be converted with @racket[->pagenode].

@examples[#:eval my-eval
(map pagenodeish? '(9.999 "index.html" |    |))
]


@defproc[
(->pagenode
[v pagenodeish?])
pagenode?]
Convert @racket[_v] to a pagenode.

@examples[#:eval my-eval
(map pagenodeish? '(symbol 9.999 "index.html" |  silly  |))
(map ->pagenode '(symbol 9.999 "index.html" |  silly  |))
]



@section{Navigation}


@defparam[current-pagetree pagetree pagetree?
          #:value #f]{
A parameter that defines the default pagetree used by pagetree navigation functions (e.g., @racket[parent-pagenode], @racket[chidren], et al.) if another is not explicitly specified. Initialized to @racket[#f].}


@defproc[
(parent
[p (or/c #f pagenodeish?)]
[pagetree pagetree? (current-pagetree)])
(or/c #f pagenode?)]
Find the parent pagenode of @racket[_p] within @racket[_pagetree]. Return @racket[#f] if there isn't one.

@examples[#:eval my-eval
(current-pagetree '(root (mama.html son.html daughter.html) uncle.html))
(parent 'son.html)
(parent "mama.html")
(parent (parent 'son.html))
(parent (parent (parent 'son.html)))
]

@defproc[
(children
[p (or/c #f pagenodeish?)]
[pagetree pagetree? (current-pagetree)])
(or/c #f pagenode?)]
Find the child pagenodes of @racket[_p] within @racket[_pagetree]. Return @racket[#f] if there aren't any.

@examples[#:eval my-eval
(current-pagetree '(root (mama.html son.html daughter.html) uncle.html))
(children 'mama.html)
(children 'uncle.html)
(children 'root)
(map children (children 'root))
]


@defproc[
(siblings
[p (or/c #f pagenodeish?)]
[pagetree pagetree? (current-pagetree)])
(or/c #f pagenode?)]
Find the sibling pagenodes of @racket[_p] within @racket[_pagetree]. The list will include @racket[_p] itself. But the function will still return @racket[#f] if @racket[_pagetree] is @racket[#f].

@examples[#:eval my-eval
(current-pagetree '(root (mama.html son.html daughter.html) uncle.html))
(siblings 'son.html)
(siblings 'daughter.html)
(siblings 'mama.html)
]


@deftogether[(

@defproc[
(previous
[p (or/c #f pagenodeish?)]
[pagetree pagetree? (current-pagetree)])
(or/c #f pagenode?)]

@defproc[
(previous*
[p (or/c #f pagenodeish?)]
[pagetree pagetree? (current-pagetree)])
(or/c #f (listof pagenode?))]
)]
Return the pagenode immediately before @racket[_p]. For @racket[previous*], return all the pagenodes before @racket[_p], in sequence. In both cases, return @racket[#f] if there aren't any pagenodes. The root pagenode is ignored.

@examples[#:eval my-eval
(current-pagetree '(root (mama.html son.html daughter.html) uncle.html))
(previous 'daughter.html)
(previous 'son.html)
(previous (previous 'daughter.html))
(previous 'mama.html)
(previous* 'daughter.html)
(previous* 'uncle.html)
]

@deftogether[(

@defproc[
(next
[p (or/c #f pagenodeish?)]
[pagetree pagetree? (current-pagetree)])
(or/c #f pagenode?)]

@defproc[
(next*
[p (or/c #f pagenodeish?)]
[pagetree pagetree? (current-pagetree)])
(or/c #f (listof pagenode?))]
)]
Return the pagenode immediately after @racket[_p]. For @racket[next*], return all the pagenodes after @racket[_p], in sequence. In both cases, return @racket[#f] if there aren't any pagenodes. The root pagenode is ignored.

@examples[#:eval my-eval
(current-pagetree '(root (mama.html son.html daughter.html) uncle.html))
(next 'son.html)
(next 'daughter.html)
(next (next 'son.html))
(next 'uncle.html)
(next* 'mama.html)
(next* 'daughter.html)
]

@section{Utilities}

@defproc[
(pagetree->list
[pagetree pagetree?])
list?
]
Convert @racket[_pagetree] to a simple list. Equivalent to a pre-order depth-first traversal of @racket[_pagetree].

@defproc[
(in-pagetree?
[pagenode pagenode?]
[pagetree pagetree? (current-pagetree)])
boolean?
]
Report whether @racket[_pagenode] is in @racket[_pagetree].

@defproc[
(path->pagenode
[p pathish?])
pagenode?
]
Convert path @racket[_p] to a pagenode — meaning, make it relative to @racket[current-project-root], run it through @racket[->output-path], and convert it to a symbol. Does not tell you whether the resultant pagenode actually exists in the current pagetree (for that, use @racket[in-pagetree?]).