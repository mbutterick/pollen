#lang scribble/manual

@(require scribble/eval pollen/cache pollen/world (for-label racket (except-in pollen #%module-begin) pollen/world pollen/ptree txexpr pollen/decode))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/ptree txexpr))

@title{Ptrees}

@defmodule[pollen/ptree]

A @italic{ptree} — short for @italic{Pollen tree} — is a hierarchical list of Pollen output files. A ptree source file has the extension @code[(format ".~a" world:ptree-source-ext)]. A ptree provides a convenient way of separating the structure of the pages from the page sources, and navigating around this structure.

Books and other long documents are usually organized in a structured way — at minimum they have a sequence of pages, but more often they have sections with subsequences within. Individual Pollen source files don't know anything about how they're connected to other files. In theory, you could maintain this information within each source file. This would be a poor use of human energy. Let the ptree figure it out.

@defproc[
(ptree?
[possible-ptree any/c])
boolean?]
Test whether @racket[_possible-ptree] is a valid ptree. It must be a @racket[txexpr?] where all elements are @racket[pnode?] and unique within @racket[_possible-ptree] (not counting the root node).

@examples[#:eval my-eval
(ptree? '(root index.html))
(ptree? '(root index.html index.html))
(ptree? '(root index.html "index.html"))
(define nested-pt '(root 1.html 2.html (3.html 3a.html 3b.html)))
(ptree? nested-pt)
(ptree? `(root index.html ,nested-pt (subsection.html more.html)))
(ptree? `(root index.html ,nested-pt (subsection.html ,nested-pt)))
]

@defproc[
(validate-ptree
[possible-ptree any/c])
ptree?]
Like @racket[ptree?], but raises a descriptive error if @racket[_possible-ptree] is invalid, and otherwise returns @racket[_possible-ptree] itself.

@examples[#:eval my-eval
(validate-ptree '(root (mama.html son.html daughter.html) uncle.html))
(validate-ptree `(root (,+ son.html daughter.html) uncle.html))
(validate-ptree '(root (mama.html son.html son.html) mama.html))
]


@defproc[
(pnode?
[possible-pnode any/c])
boolean?]
Test whether @racket[_possible-pnode] is a valid pnode (short for ``ptree node''). A pnode can be any @racket[symbol?] that is not @racket[whitespace/nbsp?] Every leaf of a ptree is a pnode. In practice, your pnodes will likely be names of output files. 

@margin-note{Pnodes are symbols (rather than strings) so that ptrees will be valid tagged X-expressions, which is a more convenient format for validation & processing.}

@examples[#:eval my-eval
(map pnode? '(symbol index.html |   silly   |))
(map pnode? '(9.999 "index.html" (p "Hello") |    |))
]


@defproc[
(pnodeish?
[v any/c])
boolean?]
Return @racket[#t] if @racket[_v] can be converted with @racket[->pnode].

@examples[#:eval my-eval
(map pnodeish? '(9.999 "index.html" |    |))
]


@defproc[
(->pnode
[v pnodeish?])
pnode?]
Convert @racket[_v] to a pnode.

@examples[#:eval my-eval
(map pnodeish? '(symbol 9.999 "index.html" |  silly  |))
(map ->pnode '(symbol 9.999 "index.html" |  silly  |))
]



@section{Navigation}


@defparam[current-ptree ptree ptree?
          #:value #f]{
A parameter that defines the default ptree used by ptree navigation functions (e.g., @racket[parent], @racket[chidren], et al.) if another is not explicitly specified. Initialized to @racket[#f].}


@defproc[
(parent
[p (or/c #f pnodeish?)]
[ptree ptree? (current-ptree)])
(or/c #f pnode?)]
Find the parent pnode of @racket[_p] within @racket[_ptree]. Return @racket[#f] if there isn't one.

@examples[#:eval my-eval
(current-ptree '(root (mama.html son.html daughter.html) uncle.html))
(parent 'son.html)
(parent "mama.html")
(parent (parent 'son.html))
(parent (parent (parent 'son.html)))
]

@defproc[
(children
[p (or/c #f pnodeish?)]
[ptree ptree? (current-ptree)])
(or/c #f pnode?)]
Find the child pnodes of @racket[_p] within @racket[_ptree]. Return @racket[#f] if there aren't any.

@examples[#:eval my-eval
(current-ptree '(root (mama.html son.html daughter.html) uncle.html))
(children 'mama.html)
(children 'uncle.html)
(children 'root)
(map children (children 'root))
]


@defproc[
(siblings
[p (or/c #f pnodeish?)]
[ptree ptree? (current-ptree)])
(or/c #f pnode?)]
Find the sibling pnodes of @racket[_p] within @racket[_ptree]. The list will include @racket[_p] itself. But the function will still return @racket[#f] if @racket[_ptree] is @racket[#f].

@examples[#:eval my-eval
(current-ptree '(root (mama.html son.html daughter.html) uncle.html))
(siblings 'son.html)
(siblings 'daughter.html)
(siblings 'mama.html)
]


@deftogether[(

@defproc[
(previous
[p (or/c #f pnodeish?)]
[ptree ptree? (current-ptree)])
(or/c #f pnode?)]

@defproc[
(previous*
[p (or/c #f pnodeish?)]
[ptree ptree? (current-ptree)])
(or/c #f (listof pnode?))]
)]
Return the pnode immediately before @racket[_p]. For @racket[previous*], return all the pnodes before @racket[_p], in sequence. In both cases, return @racket[#f] if there aren't any pnodes. The root node is ignored.

@examples[#:eval my-eval
(current-ptree '(root (mama.html son.html daughter.html) uncle.html))
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
[p (or/c #f pnodeish?)]
[ptree ptree? (current-ptree)])
(or/c #f pnode?)]

@defproc[
(next*
[p (or/c #f pnodeish?)]
[ptree ptree? (current-ptree)])
(or/c #f (listof pnode?))]
)]
Return the pnode immediately after @racket[_p]. For @racket[next*], return all the pnodes after @racket[_p], in sequence. In both cases, return @racket[#f] if there aren't any pnodes. The root node is ignored.

@examples[#:eval my-eval
(current-ptree '(root (mama.html son.html daughter.html) uncle.html))
(next 'son.html)
(next 'daughter.html)
(next (next 'son.html))
(next 'uncle.html)
(next* 'mama.html)
(next* 'daughter.html)
]

