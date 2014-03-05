#lang scribble/manual
 
@title{Pollen}

@author{Matthew Butterick}

Pollen is a publishing system that lets authors create beautiful and functional web-based books. Pollen brings together tools for writing, designing, programming, and testing. I used Pollen to make my book @link["http://practicaltypography.com"]{Butterick's Practical Typography}. You can use it to make your next web-based book — it's easy, powerful, and fun.

Skip to: Why Pollen?
Skip to: Why Racket?
Skip to: Quick tutorial

@section{Setting up Pollen}

Download Racket & install.

@code{raco pkg install pollen}

Open terminal and cd to your project directory.

Run @code{racket}.

At the prompt, type @code{(require pollen/setup)}.

Then type @code{(setup)}. 

Quit Racket with ctrl+D.

At the terminal prompt, @code{./polcom start}.



@section{Quick start}



@subsection{You had me at Hello World}

Launch DrRacket. Open a new document.

Change the top line to #lang pollen. This will be the first line of any source file where you want to invoke the Pollen language.

Type @code{Hello World} underneath.

Run the file by clicking the Run button, or typing cmd-R.

The result window will show @code{Hello World}.

Congratulations, you've made your first Pollen document.

If you like, change the text and run the file again. The new text will appear in the result window.

Pollen is a programming language that operates in text mode by default. Meaning, all plain text in the source file is considered valid input, and gets passed through intact.

@subsection{The plot thickens}

Start a new Pollen document. Remember to change the top line.

Underneath, type @code{Hello ◊(+ 1 2) Worlds}. The character before the left parenthesis is called a lozenge. Type it by [doing such and such].

Ask yourself: what are you likely to get when you run the file?

OK, now run the file.

The result will be @code{Hello 3 Worlds}. Hopefully, that's what you expected. 

Feel free to change the numbers inside the parenthesized expression and run the file again. The printed sum will change. You can also change the @code{+} sign to a @code{*} sign and make really big numbers. If you want to see your first stupid Pollen trick, type @code{Hello ◊(/ 38 57) of a World} and watch what happens.

Erase everything but the top line.

Type this: @code{
◊(define name "Roxy")

Hello ◊name}. 

What do you suppose you'll get this time?

Run the file. You'll see @code{Hello Roxy}.

The lozenge character (◊) tells Pollen to interpret what follows as code rather than plain text. This character is therefore the gateway to all the programming functions available in Pollen. In the first case, it denoted a math expression. In the second case, it denoted the definition of a variable, and then the variable itself.


@subsection{Making an HTML page with Pollen}

By default, Pollen operates in preprocessor mode. That means it evaluates all the expressions in your document, renders each as text, and then outputs the whole document as a text file. 

In this tutorial, you're going to make an HTML file. But you can use Pollen as a preprocessor for any kind of text file.

@margin-note{That means Pollen can act as a preprocessor for CSS, JavaScript, XML — and even source files for other programming languages.}


@subsection{Making an HTML page with Pollen in decoder mode}

@subsection{Using the Pollen dashboard}




@section{Why I made Pollen}

The nerds have already raced ahead to the quick tutorial. That's okay. Because software isn't just data structures and functions. It's ideas, and choices, and policies. It's design. 

I created Pollen to overcome certain tool limitations that surfaced repeatedly in my work. If you agree with my characterization of those problems, then you'll probably like the solution that Pollen offers. 

If not, you probably won't.

@subsection{The web-development problem}


I made my first web page in 1994, shortly after the web was invented. I opened my text editor (at the time, @link["http://www.barebones.com/products/bbedit/"]{BBEdit}) and pecked out @code{<html><body>Hello world</body></html>}, then loaded it in @link["http://en.wikipedia.org/wiki/Mosaic_(web_browser)"]{Mosaic}. So did a million others.

If you weren't around then, you didn't miss much. Everything about the web was horrible: the web browsers, the computers running the browsers, the dial-up connections feeding the browsers, and of course HTML itself. At that point, the desktop-software experience was already slick and refined. By comparison, using the web felt like banging rocks together.

That's no longer true. The web is now 20 years old. During that time, most parts of the web have improved dramatically — the connections are faster, the browsers are more sophisticated, the screens have more pixels. 

But one part has not: the way we make web pages. Over the years, tools promising to simplify HTML development have come and mostly gone — from PageMill to Dreamweaver. Meanwhile, true web jocks have remained loyal to the original HTML power tool: the humble text editor.

In one way, this makes sense. Web pages are mostly made of text — HTML, CSS, JavaScript, and so on — and thus the simplest way to mainpulate them is with a text editor. While HTML and CSS are not programming languages, they lend themselves to semantic and logical structure that's most easily expressed by editing them as text. Text-based editing also makes debugging and performance improvements easier.

But text-based editing is also limited. Though the underlying description of a web page is notionally human-readable, it's largely optimized to be readable by other software (namely, web browsers). HTML markup in particular is verbose and easily mistyped. And isn't it fatally dull to manage all the boilerplate, like surrounding every paragraph with @code{<p>...</p>}? Yes, it is.

For these reasons, much of web development should lend itself to automation. But in practice, tools that enable this automation have been slow to arrive, and most come hobbled with unacceptable deficiencies.

@subsubsection{Why not a content management system, like WordPress?}

I used WordPress to make the original version of @link["http://typographyforlawyers.com/"]{Typography for Lawyers} (the precursor to Butterick's Practical Typography). Even WordPress founder Matt Mullenweg @link["http://ma.tt/2010/04/typography-for-lawyers/"]{thought} it was “a cool use of WordPress for a mini-book.” Thanks, Matt. At the time, WordPress was the best tool for the job.

But at the risk of having my @link["http://gravatar.com"]{Gravatar} revoked, I'll tell you I became disenchanted with WordPress because:

It's a resource hog.

Performance is questionable.

There's always a new security problem.

No source control.

PHP.

@subsubsection{Why not a CSS preprocessor, like Sass or LESS?}

A CSS preprocessor automates the generation of CSS data. These preprocessors do save time & effort, so using one is better than not using one. My objection is that they ask you to incur much of the overhead of learning a programming language but without delivering the benefits. Because unlike a general-purpose programming language, Sass and LESS can only manipulate CSS. Better to learn a programming language that can manipulate anything.

@subsubsection{Why not a static blog generator, like Jekyll or Pelican?}



@subsubsection{Why not a dynamic templating system, like Bottle?}






@subsection{The book-publishing problem}


@section{Why I used Racket}



@subsection{Writing & editing}

@subsection{Design & layout}

@subsection{Programming}


@section{License & credits}