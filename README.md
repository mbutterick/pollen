## Pollen: the book is a program [![Build Status](https://travis-ci.org/mbutterick/pollen.svg?branch=master)](https://travis-ci.org/mbutterick/pollen)

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


Using Racket 6.0+, install from the command line:

    raco pkg install pollen
    
And update like so:

    raco pkg update --update-deps pollen
    
Official mailing list: http://groups.google.com/forum/#!forum/pollenpub

## License

LGPL

## Pull-request tips

I welcome pull requests. But accepting a PR obligates me to maintain that code for the life of Pollen. So if I seem picky about which PRs I accept — yes, because I have to be. No hard feelings.

* There’s plenty of room for improvement in the Pollen code, because every line of it has been written against the backdrop of ignorance and fallibility, mostly my own.

* I don’t necessarily prefer PRs to issues or feature requests. A good description of the problem with a working example is better than a half-baked PR. I can often fix it in less time than it would take to review the PR.

* If you want feedback on a potential PR, I recommend posting to the [Pollen mailing list](http://groups.google.com/forum/#!forum/pollenpub) rather than here. Because more people will see it.

* Small PRs are easier to accept than large ones. Large PRs should have a benefit worthy of their complexity.

* I consider every PR, but I can’t promise detailed code reviews or comments. Helpful Racketeers can be found on the [Pollen mailing list](http://groups.google.com/forum/#!forum/pollenpub), the [Racket mailing list](https://lists.racket-lang.org/), and the Racket [Slack channel](https://racket.slack.com/).

* PRs should be necessary, in the sense that the proposed change can only be accomplished by patching this repo. (Corollary:  features that can live in a separate [package](https://pkgs.racket-lang.org/) probably should.)

* PRs should avoid introducing magic behavior (aka the [principle of least astonishment](http://wiki.c2.com/?PrincipleOfLeastAstonishment)).

* PRs should forbid as little as possible. In particular, PRs should avoid enshrining personal preference as default behavior (because others will have different preferences).

* PRs should avoid reinventing features that already exist in Racket.

* I follow these principles too, because they’re virtuous habits. Still, I created Pollen as a tool for my writing and typography work. If a certain PR would negatively impact that work, I can’t accept it.

* If you’re new to Pollen or Racket, your PR is more likely to be declined, because certain things you perceive as bugs are actually features, certain things you perceive as missing are actually present, and certain limitations you perceive as surmountable are actually not. (See also point #1 re: backdrop of ignorance.)

* PRs that could have unit tests, and don’t, will be treated harshly. As they should.

* PRs that want to amend Pollen’s public interface receive the highest scrutiny.
