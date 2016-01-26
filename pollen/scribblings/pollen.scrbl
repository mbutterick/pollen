#lang scribble/manual

@(require scribble/eval pollen/world (for-label racket (except-in pollen #%module-begin) pollen/world))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen))


@title[#:style 'toc]{Pollen: the book is a program}



@author[(author+email "Matthew Butterick" "mb@mbtype.com")]

@defmodulelang[pollen]


Pollen is a publishing system that helps authors create beautiful and functional web-based books. Pollen includes tools for writing, designing, programming, testing, and publishing.

I used Pollen to create my books @link["http://practicaltypography.com"]{@italic{Butterick's Practical Typography}} and @link["http://typographyforlawyers.com"]{@italic{Typography for Lawyers}}. Sure, go take a look. Are they better than the last digital book you encountered? Yes they are. Would you like your book to look like that? If so, keep reading.

At the core of Pollen is an argument:
@itemlist[#:style 'unordered

@item{First, that digital books should be the best books we've ever had. So far, they're not even close.}

@item{Second, that because digital books are software, an author shouldn't think of a book as merely data. @bold{The book is a program.}}

@item{Third, that the way we make digital books better than their predecessors is by exploiting this programmability.}]

That's what Pollen is for.

Not that you need to be a programmer to use Pollen. On the contrary, the Pollen language is markup-based, so you can write & edit text naturally. But when you want to automate repetitive tasks, add cross-references, or pull in data from other sources, you can access a full programming language from within the text.

That language is Racket. I chose Racket because while the idea for Pollen had been with me for several years, it simply wasn't possible to build it with other languages. So if it's unfamiliar to you, don't panic. It was unfamiliar to me. Once you see what you can do with Pollen & Racket, you may be persuaded. I was.

Or, if you can find a better digital-publishing tool, use that. But I'm never going back to the way I used to work.

@local-table-of-contents[]


@include-section["installation.scrbl"]
@include-section["quick.scrbl"]
@include-section["story.scrbl"]
@include-section["big-picture.scrbl"]
@include-section["tutorial-first.scrbl"]
@include-section["tutorial-second.scrbl"]
@include-section["tutorial-third.scrbl"]
@include-section["tutorial-fourth.scrbl"]
@include-section["tutorial-mini.scrbl"]
@include-section["raco.scrbl"]
@include-section["formats.scrbl"]
@include-section["command.scrbl"]
@include-section["programming-pollen.scrbl"]
@include-section["module-reference.scrbl"]
@include-section["unstable-module-reference.scrbl"]
@include-section["more-help.scrbl"]
@include-section["acknowledgments.scrbl"]
@include-section["license.scrbl"]

@index-section[]
