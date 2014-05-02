#lang scribble/manual

@title{System overview}

A summary of the key components & concepts of the Pollen publishing system and how they fit together. If you've completed the @secref["quick-tour"], this will lend some context to what you saw. The next tutorials will make more sense if you read this first.


@section{The book is a program}

This is the core design principle of Pollen. Consistent with this principle, Pollen adopts the habits of software development in its functionality, workflow, and project management.

@itemlist[

@item{@bold{You are a programmer.} Don't panic. But let's just admit it — if your book is a program, then you are, in part, programming it. You don't have to know any programming to start using Pollen. But you'll have to be willing to learn a few programming ideas. (Those who have programmed other template-based HTML generators may have to forget a few things.)}

@item{@bold{A Pollen project consists of source files + static files.} A @italic{source file} is a file that can be compiled to produce certain output. A @italic{static file} is usable as it stands (e.g., an SVG file or webfont). Generally, the textual content of your book will live in source files, and other elements will be static files.}

@item{@bold{Source control is a good idea.} Because Pollen projects are software projects, they can be easily managed with systems for source control and collaboration, like @link["http://github.com"]{GitHub}. If you're a writer at heart, don't fear these systems — they really do make revision & edit tracking easier than it is with Word or PDF files.}

]

@section{One language, multiple dialects}

@itemlist[

@item{@bold{Everything is Racket.} The Pollen system is built entirely in the Racket programming language. Some of your source files will be in Racket. Others will be in one of the Pollen language dialects. But under the hood, everything becomes Racket code. So if you plan to do any serious work in Pollen, you'll want to learn some basics about Racket too (for instance @other-doc['(lib "scribblings/quick/quick.scrbl")]).}

@item{@bold{The Pollen language is based on Scribble.} Scribble is a variant of the Racket language that flips the usual programming syntax: instead of code with embedded textual content, a Scribble source file is text with embedded code (an idea borrowed from @link["https://en.wikipedia.org/wiki/TeX"]{TeX}). The Pollen language is adapted from Scribble. So most things that are true about Scribble are also true about Pollen (see @other-doc['(lib "scribblings/scribble/scribble.scrbl")]).}


@item{@bold{The Pollen language is divided into dialects.} The Pollen dialects share a common syntax and structure. But they're different in details that makes them better adapted to certain types of source files (for instance, one dialect of Pollen understands Markdown; the others don't). You can use whichever suits the task at hand.}

]


@section{Development environment}

The Pollen development environment has two main pieces: the DrRacket code editor, and the Pollen project server.

@itemlist[

@item{@bold{Edit source files with DrRacket.} DrRacket is Racket's GUI code editor. Sure, you can also use a generic text editor. But DrRacket lets you immediately run your source file and see if it works. I know your favorite programming language doesn't have that. But trust me, it's very convenient.} 

@item{@bold{Render & preview web pages with the Pollen project server.} Pollen has a built-in development web server called the @italic{project server}. After you start the project server, you can preview & test your web pages within a web browser with maximum accuracy. Everything is rendered to static HTML so you can see exactly what you'll get.}

@item{@bold{One directory for everything.} Rather than separating your source files and static files, Pollen keeps them all in one directory so it's simple to make links between them.}

]



@section{A special data structure for HTML}

X-expression

Tags can be tags

Or tags can be functions


@section{Preprocessor source files}

Lower level of abstraction

Work directly

Text output

Any kind of file!


@section{Templated source files}

Higher level of abstraction

Work indirectly

X-expression output

Markdown

Markup

Templates

@section{Utility source files}

Scribble

Null


@section{Pagetrees}

Navigation

Hierarchy

Subsetting

Build script





@section{Build & deploy}

Pollen is not a production server

Render & clone