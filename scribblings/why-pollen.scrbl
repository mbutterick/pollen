#lang scribble/manual

@title{Why I made Pollen}

The nerds have already raced ahead to the quick tutorial. That's okay. Because software isn't just data structures and functions. It's ideas, and choices, and policies. It's design. 

I created Pollen to overcome certain tool limitations that surfaced repeatedly in my work. If you agree with my characterization of those problems, then you'll probably like the solution that Pollen offers. 

If not, you probably won't.

@section{The web-development problem}


I made my first web page in 1994, shortly after the web was invented. I opened my text editor (at the time, @link["http://www.barebones.com/products/bbedit/"]{BBEdit}) and pecked out @code{<html><body>Hello world</body></html>}, then loaded it in @link["http://en.wikipedia.org/wiki/Mosaic_(web_browser)"]{Mosaic}. So did a million others.

If you weren't around then, you didn't miss much. Everything about the web was horrible: the web browsers, the computers running the browsers, the dial-up connections feeding the browsers, and of course HTML itself. At that point, the desktop-software experience was already slick and refined. By comparison, using the web felt like banging rocks together.

That's no longer true. The web is now 20 years old. During that time, most parts of the web have improved dramatically — the connections are faster, the browsers are more sophisticated, the screens have more pixels. 

But one part has not: the way we make web pages. Over the years, tools promising to simplify HTML development have come and mostly gone — from PageMill to Dreamweaver. Meanwhile, true web jocks have remained loyal to the original HTML power tool: the humble text editor.

In one way, this makes sense. Web pages are mostly made of text — HTML, CSS, JavaScript, and so on — and thus the simplest way to mainpulate them is with a text editor. While HTML and CSS are not programming languages, they lend themselves to semantic and logical structure that's most easily expressed by editing them as text. Text-based editing also makes debugging and performance improvements easier.

But text-based editing is also limited. Though the underlying description of a web page is notionally human-readable, it's largely optimized to be readable by other software (namely, web browsers). HTML markup in particular is verbose and easily mistyped. And isn't it fatally dull to manage all the boilerplate, like surrounding every paragraph with @code{<p>...</p>}? Yes, it is.

For these reasons, much of web development should lend itself to automation. But in practice, tools that enable this automation have been slow to arrive, and most come hobbled with unacceptable deficiencies.

@subsection{Why not a content management system, like WordPress?}

I used WordPress to make the original version of @link["http://typographyforlawyers.com/"]{Typography for Lawyers} (the precursor to Butterick's Practical Typography). Even WordPress founder Matt Mullenweg @link["http://ma.tt/2010/04/typography-for-lawyers/"]{thought} it was “a cool use of WordPress for a mini-book.” Thanks, Matt. At the time, WordPress was the best tool for the job.

But at the risk of having my @link["http://gravatar.com"]{Gravatar} revoked, I'll tell you I became disenchanted with WordPress because:

It's a resource hog.

Performance is questionable.

There's always a new security problem.

No source control.

PHP.

@subsection{Why not a CSS preprocessor, like Sass or LESS?}

A CSS preprocessor automates the generation of CSS data. These preprocessors do save time & effort, so using one is better than not using one. My objection is that they ask you to incur much of the overhead of learning a programming language but without delivering the benefits. Because unlike a general-purpose programming language, Sass and LESS can only manipulate CSS. Better to learn a programming language that can manipulate anything.

@subsection{Why not a static blog generator, like Jekyll or Pelican?}



@subsection{Why not a dynamic templating system, like Bottle?}



