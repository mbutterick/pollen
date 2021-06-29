## Pollen: the book is a program [![Build Status](https://github.com/mbutterick/pollen/workflows/CI/badge.svg)](https://github.com/mbutterick/pollen/actions) [![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.0-4baaaa.svg)](CODE_OF_CONDUCT.md) 

A book-publishing system written in [Racket](http://racket-lang.org). This is the software I use to publish & maintain my web-based books [Beautiful Racket](http://beautifulracket.com), [Practical Typography](http://practicaltypography.com), and [Typography for Lawyers](http://typographyforlawyers.com).

If you think documents should be programmable, you’ll love it.  
If not, you can move along.

Pollen gives you access to a full programming language (Racket) with a text-based syntax that makes it easy to embed code within your documents.

* [Quick tour](http://pkg-build.racket-lang.org/doc/pollen/quick-tour.html)
* [Pollen as a text preprocessor (for CSS, etc.)](http://pkg-build.racket-lang.org/doc/pollen/first-tutorial.html)
* [Pollen for Markdown authoring](http://pkg-build.racket-lang.org/doc/pollen/second-tutorial.html)
* [Pollen for free-form markup authoring](http://pkg-build.racket-lang.org/doc/pollen/third-tutorial.html)
* [Pollen for multiple-output publishing](http://pkg-build.racket-lang.org/doc/pollen/fourth-tutorial.html)
* [Full docs](http://pkg-build.racket-lang.org/doc/pollen)


Using Racket 6.3+, install from the command line:

    raco pkg install pollen
    
And update like so:

    raco pkg update --update-deps pollen
    
Official discussion area: https://github.com/mbutterick/pollen-users

## License

MIT

## Project status

Actively developed, though the pace has slowed now that Pollen is arguably feature complete and stable. I use it almost every day so it's not going anywhere. But I have no plans to substantially enlarge or extend it.
