#lang scribble/manual

@(require "mb-tools.rkt" scribble/eval pollen/cache pollen/world (for-label racket (except-in pollen #%module-begin) pollen/world txexpr pollen/decode pollen/file sugar pollen/pagetree))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/pagetree txexpr))

@title{Pagetree}

@defmodule[pollen/pagetree]

Books and other long documents are usually organized in a structured way — at minimum they have a sequence of pages, but more often they have sections with subsequences within. Individual pages in a Pollen project don't know anything about how they're connected to other pages. In theory, you could maintain this information within the source files. But this would be a poor use of human energy.

Instead, use a pagetree. A @italic{pagetree} is a simple abstraction for defining & working with sequences of @italic{pagenodes}. Typically these pagenodes will be the names of output files in your project. 

``So it's a list of web-page filenames?'' Sort of. When I think of a web page, I think of an actual file on a disk. Keeping with Pollen's orientation toward dynamic rendering, pagenodes may — and often do — refer to files that don't yet exist. Moreover, by referring to output names rather than source names, you retain the flexibility to change the kind of source associated with a particular pagenode (e.g., from preprocessor source to Pollen markup).

Pagetrees can be flat or hierarchical. A flat pagetree is just a @seclink["Lists__Iteration__and_Recursion"
#:doc '(lib "scribblings/guide/guide.scrbl")]{list} of pagenodes. A hierarchical pagetree can also contain recursively nested lists of pagenodes. But you needn't pay attention to this distinction, as the pagetree functions don't care which kind you use. Neither do I.

Pagetrees surface throughout the Pollen system. They're primarily used for navigation — for instance, calculating ``previous,'' ``next,'' or ``up'' links for a given page. A special pagetree, @racketfont{index.ptree}, is used by the project server to order the files in a dashboard. Pagetrees can also be used to define batches of files for certain operations, for instance @secref["raco_pollen_render" #:doc '(lib "pollen/scribblings/pollen.scrbl")]. You might find other uses for them too.



@section{Making pagetrees with a source file}

A pagetree source file either starts with @code{#lang pollen} and uses the @racketfont{@(format ".~a" world:pagetree-source-ext)} extension, or starts with @code{#lang pollen/ptree} and then can have any file extension. 

Unlike other Pollen source files, since the pagetree source is not rendered into an output format, the rest of the filename is up to you.

Here's a flat pagetree. Each line is considered a single pagenode (blank lines are ignored). Notice that no Pollen command syntax nor quoting is needed within the pagetree source:

@fileblock["flat.ptree" @codeblock{
#lang pollen

index.html
introduction.html
main_argument.html
conclusion.html
}]

And here's the output in DrRacket:

@repl-output{'(pagetree-root index.html introduction.html main_argument.html conclusion.html)}

Keeping with usual Pollen policy, this is an @seclink["X-expressions" #:doc '(lib "pollen/scribblings/pollen.scrbl")]{X-expression}. The @racket[pagetree-root] is just an arbitrary tag that contains the pagetree.

Upgrading to a hierarchical pagetree is simple. The same basic rule applies — one pagenode per line. But this time, you add Pollen command syntax: a lozenge @litchar{◊} in front of a pagenode marks it as the top of a nested list, and the sub-pagenodes of that list go between @litchar{@"{"} curly braces @litchar{@"}"}, like so:

@fileblock["hierarchical.ptree" @codeblock{
#lang pollen

toc.html
◊first-chapter.html{
    foreword.html
    introduction.html}
◊second-chapter.html{
    ◊main-argument.html{
        facts.html
        analysis.html}
    conclusion.html}
bibliography.html
}]

The output of our hierarchical pagetree:

@repl-output{'(pagetree-root toc.html (first-chapter.html foreword.html introduction.html) (second-chapter.html (main-argument.html facts.html analysis.html) conclusion.html) bibliography.html)}

One advantage of using a source file is that when you run it in DrRacket, it will automatically be checked using @racket[validate-pagetree], which insures that every element in the pagetree meets @racket[pagenode?], and that all the pagenodes are unique. 

This pagetree has a duplicate pagenode, so it won't run:

@fileblock["duplicate-pagenode.ptree" @codeblock{
#lang pollen

index.html
introduction.html
main_argument.html
conclusion.html
index.html
}]

Instead, you'll get an error:

@errorblock{validate-pagetree: members-unique? failed because item isn’t unique: (index.html)}

@section{Making pagetrees by hand}

Experienced programmers may want to know that because a pagetree is just an X-expression, you can synthesize a pagetree using any Pollen or Racket tools for making X-expressions. For example, here's some Racket code that generates the same pagetree as the @racketfont{flat.ptree} source file above:

@fileblock["make-flat-ptree.rkt" @codeblock{
#lang racket
(require pollen/pagetree)
(define node-names '(index introduction main_argument conclusion))
(define pt `(pagetree-root 
  ,@"@"(map (λ(n) (string->symbol (format "~a.html" n))) node-names)))
(if (pagetree? pt) pt "Oops, not a pagetree")
}]

Note that you need to take more care when building a pagetree by hand. Pagenodes are symbols, not strings, thus the use of @racket[string->symbol] is mandatory. One benefit of using a pagetree source file is that it takes care of this housekeeping for you.

@section{Using pagetrees for navigation}

Typically you'll call the pagetree-navigation functions from inside templates, using the special variable @racket[here] as the starting point. For more on this technique, see @secref["Pagetree_navigation" #:tag-prefixes '( "tutorial-2")].

@section{Using @racketfont{index.ptree} in the dashboard}

When you're using the project server to view the files in a directory, the server will first look for a file called @racketfont{index.ptree}. If it finds this pagetree file, it will use it to build the dashboard. If not, then it will synthesize a pagetree using a directory listing. For more on this technique, see @secref["Using_the_dashboard" #:doc '(lib "pollen/scribblings/pollen.scrbl")].

@section{Using pagetrees with @exec{raco pollen render}}

The @exec{raco pollen render} command is used to regenerate an output file from its source. If you pass a pagetree to @exec{raco pollen render}, it will automatically render each file listed in the pagetree.

For instance, many projects have auxiliary pages that don't really belong in the main navigational flow. You can collect these pages in a separate pagetree:

@fileblock["utility.ptree" @codeblock{
#lang pollen

404-error.html
terms-of-service.html
webmaster.html
[... and so on]
}]

Thus, when you're using pagetree-navigation functions within a template, you can use your main pagetree, and restrict the navigation to the main editorial content. But when you render the project, you can pass both pagetrees to @exec{raco pollen render}.

For more on this technique, see @secref["raco_pollen_render" #:doc '(lib "pollen/scribblings/pollen.scrbl")].


@section{Functions}

@subsection{Predicates & validation}


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



@subsection{Navigation}


@defparam[current-pagetree pagetree pagetree?]{
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

@subsection{Utilities}

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