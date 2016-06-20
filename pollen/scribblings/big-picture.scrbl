#lang scribble/manual

@title[#:tag "big-picture"]{The big picture}

A summary of the key components & concepts of the Pollen publishing system and how they fit together. If you've completed the @secref["quick-tour"], this will lend some context to what you saw. The upcoming @seclink["first-tutorial"]{tutorials} will make more sense if you read this first.


@section[#:tag "the-book-is-a-program"]{The book is a program}

This is the core design principle of Pollen. Consistent with this principle, Pollen adopts the habits of software development in its functionality, workflow, and project structure.

@itemlist[

@item{@bold{You are a programmer.} Don't panic. But let's just admit it — if your book is a program, then you are, in part, programming it. You don't have to know any programming to start using Pollen. But you'll have to be willing to learn a few programming ideas. (And those who have used other template-based HTML generators may have to forget a few things.)}

@item{@bold{A Pollen project consists of source files + static files.} A @italic{source file} is a file that can be compiled to produce certain output. A @italic{static file} is usable as it stands (e.g., an SVG file or webfont). Generally, the textual content of your book will live in source files, and other elements will be static files.}

@item{@bold{Source control is a good idea.} Because Pollen projects are software projects, they can be easily managed with systems for source control and collaboration, like @link["http://github.com"]{GitHub}. If you're a writer at heart, don't fear these systems — the learning curve is repaid by revision & edit tracking that's much easier than it is with Word or PDF files.}

]

@section{One language, multiple dialects}

@itemlist[

@item{@bold{Everything is Racket.} The Pollen system is built entirely in the Racket programming language. Some of your source files will be in Racket. Others will be in one of the Pollen language dialects. But under the hood, everything becomes Racket code. So if you plan to do any serious work in Pollen, you'll want to learn some basics about Racket too (for instance @other-doc['(lib "scribblings/quick/quick.scrbl")]).}

@item{@bold{The Pollen language is based on Scribble.} Scribble is a variant of the Racket language that flips the usual programming syntax: instead of code with embedded textual content, a Scribble source file is text with embedded code (an idea borrowed from @link["https://en.wikipedia.org/wiki/TeX"]{TeX}). The Pollen language is adapted from Scribble. So most things that are true about Scribble are also true about Pollen (see @other-doc['(lib "scribblings/scribble/scribble.scrbl")]).}


@item{@bold{The Pollen language is a set of dialects.} The Pollen dialects share a common syntax and structure. But they're different in details that makes them better adapted to certain types of source files (for instance, one dialect of Pollen understands Markdown; the others don't). Use whichever suits the task at hand.}

]


@section{Development environment}

The Pollen development environment has three main pieces: the DrRacket code editor, the project server, and the command line.

@itemlist[

@item{@bold{Edit source files with DrRacket.} DrRacket is Racket's GUI code editor. Sure, you can also use a generic text editor. But DrRacket lets you immediately run your source and see if it works.} 

@item{@bold{Preview & test web pages with the Pollen project server.} Pollen has a built-in development web server called the @seclink["Using_the_project_server"]{@defterm{project server}}. After you start the project server, you can preview your web pages within any web browser, allowing you to test them with maximum accuracy.}

@item{@bold{Write the docs.} The project server can recognize and render Scribble files, so you can use it as a previewing tool while you're writing your documentation.}

@item{@bold{Render & deploy from the command line.} Your Pollen project ultimately gets rendered to a set of static files (usually HTML and related assets). This can be controlled from the command line, so you can integrate it into other scripts.} 


]


@section{A special data structure for HTML}

Unlike other programming languages, Pollen (and Racket) internally represent HTML with something called @secref["X-expressions"]. An X-expression is simply a list that represents an HTML @defterm{element}, meaning a thing with an opening tag, a closing tag, and content in between. Like HTML elements, X-expressions can be nested. Unlike HTML elements, X-expressions have no closing tag, they use parentheses to denote the start and end, and text elements are put inside quotes.

For example, consider this HTML element:

@nested[#:style 'code-inset]{@verbatim{<body><h1>Hello world</h1><p>Nice to <i>see</i> you.</p></body>}}

As a Racket X-expression, this would be written:

@nested[#:style 'code-inset]{@verbatim{(body (h1 "Hello world") (p "Nice to " (i "see") " you."))}}

More will be said about X-expressions. But several advantages should be evident already. First, without the redundant angle brackets, the X-expression is arguably more readable than the equivalent HTML. Second, an X-expression is preferable to treating HTML as a simple string, because it preserves the internal structure of the element. Third, an X-expression is a native data type in Racket.


@section{Pollen command syntax}

As mentioned above, a Pollen source file is not code with text embedded in it, but rather text with code embedded. (See @secref["pollen-command-syntax"] for more.)

@itemlist[

@item{@bold{If you can write text, you can program in Pollen.} Really. As you already found out in the @secref["quick-tour"], this is a valid Pollen program:
@codeblock{
#lang pollen
Bonjour, tout le monde: comment ça va?
}}

@item{@bold{Commands start with ◊.} A simple rule: if something in a Pollen source file starts with @litchar{◊}, it's treated as a command; otherwise it's treated as ordinary text.}

@item{@bold{Write commands in Pollen mode or Racket mode.} Commands can use two equivalent notation systems: either Pollen's text-oriented command syntax, or standard Racket syntax.}


@item{@bold{Everything in Racket is in Pollen too.} Pollen isn't a watered-down ``template language.'' Racket is a fully provisioned programming language, and every Racket function is available in Pollen.}

]


@section{The preprocessor}

The @italic{preprocessor} is the simplest processing mode in Pollen. 

@itemlist[

@item{@bold{Text output.} The preprocessor scans the source for any Pollen commands, resolves them, and outputs the whole file as text.}

@item{@bold{Work with any text file.} You can use the preprocessor with HTML, CSS, Markdown, JavaScript, XML, SVG, or any other text-based file (including source files of other programming languages). I hope this blows your mind a teeny bit.}

@item{@bold{Start quickly.} Because it works with any text file, the preprocessor is an easy way to try out Pollen, because you can mix it into your workflow on an existing project, or even just one file.}

]

@section{Templated source files}

If you want to apply a particular page format to multiple sources of content — as you would in a book — you can use Pollen @defterm{templates}.

@itemlist[

@item{@bold{Templates can be any format.} Usually Pollen templates will be HTML. But they don't have to be. Templates can generate any kind of file — either text-based (XML) or not (PDF).}

@item{@bold{Markdown authoring mode.} Pollen has a built-in Markdown parser, so you can import Markdown sources into a Pollen publication.}

@item{@bold{Pollen markup.} Pollen markup allows you the freedom to define your own markup tags and attach behavior to them.}

@item{@bold{Mix source types.} Every text source is converted to an X-expression before going into a template. So it's fine to have multiple dialects of source files in one project.}

]



@section{Pagetrees}

Similar to a table of contents, a @defterm{pagetree} is a special Pollen source file that gets turned into a hierarchical list of pages.


@itemlist[

@item{@bold{Navigation.} Pagetrees are used to provide navigation links within HTML templates (like previous, next, up, top).}

@item{@bold{Organization.} Multiple pagetrees can be used to divide your project into subsets of pages that should be treated separately.}


]



