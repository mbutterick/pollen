#lang scribble/manual

@(require scribble/eval pollen/world (for-label racket (except-in pollen #%module-begin) pollen/world))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen))


@title[#:style 'toc]{Pollen: the book is a program}



@author[(author+email "Matthew Butterick" "mb@mbtype.com")]

Pollen is a publishing system that helps authors create beautiful and functional web-based books. Pollen includes tools for writing, designing, programming, testing, and publishing.

I used Pollen to create my book @link["http://practicaltypography.com"]{Butterick's Practical Typography}. Sure, go take a look. Is it better than the last digital book you encountered? Yes it is. Would you like your book to look like that? If so, keep reading.

Pollen is built around two ideas. First, that digital books should be the best books we've ever had. (So far, they're not even close.) Second, that if digital books are software, an author shouldn't think of their book as merely data. The book is a program.

Not that you need to be a programmer to use Pollen. On the contrary, the Pollen language is markup-based, so you can write & edit text naturally. But when you want to automate repetitive tasks, add cross-references, or pull in data from other sources, you can access a full programming language from within the text.

That language is Racket. I chose Racket because while the idea for Pollen had been with me for several years, it simply wasn't possible to build it with other languages. So if it's unfamiliar to you, don't panic. It was unfamiliar to me. Once you see what you can do with Pollen & Racket, you may be persuaded. I was.

Or, if you can find a better digital book-publishing tool, use that. Personally, I'm never going back to the way I used to work.

@local-table-of-contents[]


@section{Installation}

Install Racket, which includes DrRacket.

Install Pollen from the command line:
@verbatim{raco pkg install pollen}

After that, you can update the package from the command line:
@verbatim{raco pkg update pollen}



@section{Pollen source formats}

@defmodulelang[pollen]

This puts Pollen into automatic mode, where the source file is interpreted according to the file extension. 

If the file extension is ``@(format ".~a" world:markup-source-ext)'', the source is interpreted as @racket[pollen/markup].

If the file extension is ``@(format ".~a" world:preproc-source-ext)'', the source is interpreted as @racket[pollen/pre] (``pre'' stands for ``preprocessor'').

If the file extension is ``@(format ".~a" world:markdown-source-ext)'', the source is interpreted as @racket[pollen/markdown].

@defmodulelang[pollen/markup]


@defmodulelang[pollen/pre]

@defmodulelang[pollen/markdown]

@include-section["module-reference.scrbl"]


@section{License & source code}

This module is licensed under the LGPL.

Source repository at @link["http://github.com/mbutterick/pollen"]{http://github.com/mbutterick/pollen}. Suggestions & corrections welcome.

