#lang scribble/manual

@(require (for-label pollen/world))

@title[#:tag "first-tutorial"]{First tutorial}

In this tutorial, you'll use Pollen to make a single HTML page with a poem. You'll learn about:

@itemlist[

@item{The relationship of Racket & Pollen}

@item{DrRacket}

@item{The project server}

@item{The preprocessor}

]

If you want the shortest possible introduction to Pollen, try the @secref["quick-tour"].

@section[#:tag-prefix "tutorial-1"]{Prerequisites}

I'm going to assume that you've already installed Racket and Pollen. If not, do that now.

I'm also going to assume you know the basics of using a command line to run programs and navigate the file system using commands like @tt{cd} and @tt{ls}. On Mac OS X, your command-line program is called Terminal; on Windows it's the Windows Command Processor.

@section{The relationship of Racket & Pollen}

As I mentioned in the @secref["big-picture"], Pollen is built using Racket, and everything in Pollen ultimately becomes Racket code. If you're comfortable with that idea, you may move along. 

But if not, or if you're just a curious character:

One of the key features of Racket as a programming language is that it provides tools to create @italic{other} programming languages. These languages might look & behave @link["http://docs.racket-lang.org/ts-guide/index.html"]{like Racket}. Or they @link["http://hashcollision.org/brainfudge/"]{might not}. These languages might serve a general purpose, but more often they're specialized for a particular purpose, in which case they're known as @italic{domain-specific languages}, or @italic{DSLs}. 

@margin-note{Racket exploits the fact that under the hood, all programming languages are basically doing the same thing. (CS jocks know this more formally as a side effect of @link["https://en.wikipedia.org/wiki/Turing_completeness"]{Turing completeness}.) Racket starts with the most general expression of a Turing-complete language — called @link["https://en.wikipedia.org/wiki/Lambda_calculus"]{the lambda calculus} — and lets users build on that. In most programming languages, you can build functions, classes, and modules. But in Racket, you can alter anything about the language.}

If you find this a strange idea, you're not alone. Most programmers — and until recently, me too — have never made or used DSLs. If you have a programming problem to solve, you start with a general-purpose language like Python or Java or Ruby, and go from there. Nothing wrong with that. 

But programming languages contain their own design choices and compromises. Sometimes the problem at hand is best solved by manipulating the language at a deeper level. When you make a DSL, you're still programming in the underlying language, but doing so at a point of higher leverage.

Pollen is a DSL implemented in Racket. It is a close cousin of @other-doc['(lib "scribblings/scribble/scribble.scrbl")], another Racket DSL, which was designed for writing Racket documentation. The key feature of Scribble, and thus also of Pollen, is that it's text-based. Meaning, whereas most languages have source files made of code with text embedded within, Pollen's source files are text with code embedded within.

Moreover, Pollen is meant to be a small step away from Racket — you can think of it as a more convenient notation system for Racket code, similar to how Markdown is a more convenient notation for HTML. But unlike Markdown, which only lets you access a subset of HTML, anything that can be done in Racket can also be done in Pollen. 

As you work more with Pollen, you'll pick up more about how Pollen corresponds to Racket (see @secref["reader"]) and easily be able to convert commands from one system to the other. In later tutorials, you'll see how larger Pollen projects are made out of both Pollen and Racket source files.

But in smaller projects, like this one, you can just use Pollen.

@section{Starting a new file in DrRacket}

DrRacket is the IDE for the Racket programming language, and other languages made with Racket (like Pollen). IDE stands for ``Integrated Development Environment,'' which is a fancy phrase for ``a nice place to edit and run your code.'' DrRacket is installed as part of the core Racket distribution.

@margin-note{If you've worked with languages like Perl, Python, or Ruby, you may be more familiar with using a general-purpose text editor to edit your code, and then running your program at the command line. You can do that with Racket too. But DrRacket is a considerately designed tool. I recommend it. For these tutorials, I'll assume you're using DrRacket. If you insist on using the command line, I trust you to figure out what you need to do to keep up.}

Launch DrRacket. Start a new file. The code in the file will look like this:

@racketmod[racket]

Within the main window, you should also see an @italic{interactions window}, which shows the output of the current file, and starts out looking something like this (details, like the version number, will vary):

@verbatim{
Welcome to DrRacket, version 6.0.1.6--2013-11-26(-/f) [3m].
Language: racket; memory limit: 1000 MB.
> }

If you don't see the interactions window, select @menuitem["View"
"Show Interactions"] from the menu.

@subsection{Setting the @racketfont{#lang} line}

The first line of every Racket source file, and every Pollen source file, is called the @italic{@racketfont{#lang} line}. The @racketfont{#lang} line identifies the language used to interpret the rest of the file.  

@margin-note{For more about the @racketfont{#lang} line, see @secref[#:doc '(lib "scribblings/guide/guide.scrbl") "hash-lang"].}

When you start a new Pollen source file in DrRacket, you'll need to change the @racketfont{#lang} line to the Pollen language. The simplest way is to change the first line to this:

@racketmod[pollen]

Now run your file by clicking the @onscreen["Run"] button in the upper-right corner, or select @menuitem["Racket" "Run"] from the menu. You'll get something like:

@verbatim{
Welcome to DrRacket, version 6.0.1.6--2013-11-26(-/f) [3m].
Language: pollen; memory limit: 1000 MB.
> 
}

Notice that the language is now reported as @racketfont{pollen}. If you like, change the @racketfont{#lang} line to this:

@nested[#:style 'code-inset]{@verbatim{
#lang pollenxyz}}

Then click @onscreen["Run"] again. DrRacket will print an error:

@verbatim{@racketerror{Module Language: invalid module text
@(linebreak)standard-module-name-resolver: collection not found ...}}

Why? Because there's no language called @racketfont{pollenxyz}. Switch it back to @racketfont{pollen} and let's move on.

@subsection{Putting in the text of the poem}

Here's a short, bad poem I wrote about CSS.

@verbatim{
The margin is 42em.
The border is red.
The padding is 15em.
The border is too.
}

Paste the text of this poem into your DrRacket editing window, below the @racketfont{#lang} line, so it looks like this:

@nested[#:style 'code-inset]{@verbatim{
#lang pollen

The margin is 42em.
The border is red.
The padding is 15em.
The border is too.}}

@onscreen["Run"] the file again. In the interactions window, you'll see:

@racketvalfont{
The margin is 8em.
@(linebreak)The border is blue.
@(linebreak)The padding is 2em.
@(linebreak)The border is too.}

This shows you something important: by default, any plain text in a Pollen source file is simply printed as written when you @onscreen["Run"] the file (minus the @racketfont{#lang} line, which is just for Racket's benefit). If you like, edit the text of the poem and click @onscreen["Run"] again. You'll see the updated text printed in the interactions window.

@subsection{Saving & naming your source file}

File naming in Pollen is consequential.

Ultimately, every Pollen source file in your project will be @italic{rendered} into an output file. Each Pollen source file corresponds to one output file. @bold{The name of this output file will be the name of the source file minus the Pollen source extension.} So a source file called @racketfont{file.txt.pp} will become @racketfont{file.txt}.

Thus, to build the name of a source file, we take the name we want for the output file and add the appropriate Pollen file extension. Different Pollen source files use different extensions — but more about that later. For now, the extension you'll use for your source is @racketfont{.pp}.

In this case, let's say we want to end up with a file called @racketfont{poem.html}. Therefore, the name of our source file needs to be:

the output name @racketfont{poem.html} + the source extension @racketfont{.pp} = @racketfont{poem.html.pp} 

(If you want to name the file @racketfont{something-else.html.pp}, be my guest. There's no magic associated with the prefix.)

@margin-note{You're welcome to change the name of your source files from the desktop. On Mac OS X and Windows, however, the desktop interface often hides file extensions, so check the properties of the file afterward to make sure you got the name you expected.}

In a convenient location (e.g., your home directory or the desktop) create a new directory for your project called @racketfont{tutorial}. In this new directory, save your DrRacket file as @racketfont{poem.html.pp}.

@filebox["/path/to/tutorial/poem.html.pp"]{@verbatim{
#lang pollen

The margin is 42em.
The border is red.
The padding is 15em.
The border is too.}}


@section{Using the project server}

The project server is a web server built into Pollen. Just as DrRacket lets you run individual files and see if they work as you expect, the project server lets you preview and test your project as a real website. While working on your Pollen project, you may find it convenient to have DrRacket open on half your screen, and on the other half, a web browser pointing at the project server.

@image["scribblings/project-server.png" #:scale 0.7]

``Why can't I just open the HTML files directly in my browser?'' If you want to keep making web pages the way we did in 1996, go ahead. But that approach has several shortcomings. First, when you open files directly in your browser, you're cruising the local filesystem, and absolute URLs (the kind that start with a @litchar{/}) won't work. Second, if you want to test your website on devices other than your own machine — well, you can't. Third, you have to render your HTML files in advance, whereas the project server is clever about doing this dynamically. 

So use the project server.

A note about security. The project server isn't intended for real-world use, but rather as a development tool. That said, once you start the project server, it's an actual web server running on your machine, and it will respond to requests from any computer. If you want to limit traffic to your local network, or certain machines on your local network, it's your job — not mine — to configure your firewall or other network security measures accordingly.



@subsection{Starting the project server with @racketfont{raco pollen}}

Before we start the project server, a word about the @racketfont{raco pollen} command. 

When you installed Racket, Racket installed a utility program called @racketfont{raco}. This name is short for @bold{Ra}cket @bold{co}mmand, and @racketfont{raco} acts as a hub for — you guessed it — Racket commands. You used it when you first installed Pollen:

@verbatim{
> raco pkg install pollen
}

The first argument after @racketfont{raco} is the subcommand. For instance, @racketfont{raco pkg ...} lets you install, update, and remove packages like so:

@verbatim{
> raco pkg update pollen
> raco pkg remove pollen
}

Likewise, @racketfont{raco pollen} lets you issue commands relevant to Pollen, like starting the project server. (See @secref["raco-pollen"] for a full description of available commands.) 

Now we'll start the project server. Go to your command line and enter the following:

@verbatim{
> cd /path/to/tutorial
> raco pollen start}

@margin-note{Windows users, I'll trust you to convert @racketfont{raco} into the appropriate command for your system — assuming defaults, it's likely to be @racketfont{"C:\Program Files\Racket\raco"} (include the surrounding quotes in the command).}

After a moment, you'll see a startup message like this:

@verbatim{
Welcome to Pollen 0.001 (Racket 6.x.x.x)
Project root is /path/to/tutorial
Project server is http://localhost:8080 (Ctrl-C to exit)
Project dashboard is http://localhost:8080/index.ptree
Ready to rock}

@italic{Project root} means the directory that the project server was started in, and which it's treating as its root directory. Any absolute URLs (i.e., those beginning with @litchar{/}) will resolve into this directory. So a URL like @racketfont{/styles.css} will impliedly become @racketfont{/path/to/tutorial/styles.css}. 

If you use the bare command @racketfont{raco pollen start}, the project server will start in the current directory. But if you want to start the project server elsewhere, you can add that directory as an argument like this:

@verbatim{
> raco pollen start /some/other/path
}

The next line of the startup message tells you that the web address of the project server is @racketfont{http://localhost:8080}. This is the address you put into your web browser to test your project. If you're unfamiliar with this style of URL, @racketfont{localhost} refers to your own machine, and @racketfont{8080} is the network port where the project server will respond to browser requests.

If you want to access the project server from a different machine, you can't use @racketfont{localhost}. But you can use the IP address of the machine running the project server (e.g., @racketfont{http://192.168.1.10:8080}) or any name for that machine available through local DNS (e.g., @racketfont{http://mb-laptop:8080}).

Though port @racketfont{8080} is the default, you can start the project server on any port you like by adding it as an argument to @racketfont{raco pollen start}:

@verbatim{
> raco pollen start /path/to/tutorial
> raco pollen start /path/to/tutorial 8088
}

@margin-note{You can also change the default port by altering @racket[world:default-port], or parameterizing it with @racket[world:current-server-port].}

Note that when you pass a port argument, you also have to pass a path argument. If you want the project server to start in the current directory, you can use the usual @litchar{.} shorthand:

@verbatim{
> cd /path/to/tutorial
> raco pollen start 8088 
@racketerror{/path/to/tutorial/8088 is not a directory}
> raco pollen start . 8088 
}

@margin-note{You can run multiple project servers simultaneously. Just start them on different ports so they don't conflict with each other.}

Your terminal window will report status and error messages from the project server as it runs. Use @onscreen{Ctrl-C} to stop the server. 


@subsection{Using the dashboard}

For each directory in your project, starting at the top, the project server displays a @italic{dashboard} in your web browser. The dashboard gives you an overview of the files in the directory, and links to view them.

The address of the top-level dashboard is @racketfont{http://localhost:8080/index.ptree}. Other dashboards follow the same pattern (e.g., @racketfont{http://localhost:8080/path/to/dir/index.ptree}.) 

Note that the dashboard is @bold{not} at @racketfont{http://localhost:8080/} or its equivalent, @racketfont{http://localhost:8080/index.html}. Why? So it doesn’t interfere with any @racketfont{index.html} that you may want to put in your project.

Thus, @racketfont{index.ptree}. The @racketfont{.ptree} extension is short for @italic{pagetree}. In Pollen, a pagetree is a hierarchical list of pages. We'll do more with pagetrees in a later tutorial. For now, just be aware that to generate the dashboard, the project server will first look for an actual @racketfont{index.ptree} file in each directory. If it doesn't find one, it will generate a pagetree from a listing of files in the directory.

Let's look at the root-level dashboard for our project. First, make sure your project server is running:

@verbatim{
> cd /path/to/tutorial
> raco pollen start
}

Then, in your web browser, visit @link["http://localhost:8080/index.ptree"]{@racketfont{http://localhost:8080/index.ptree}}. 

You should see something like this:

@image["scribblings/dashboard.png" #:scale 1]

The top line tells us that we're in the root directory of the project. We didn't make an explicit @racketfont{index.ptree} file, so the project server just shows us a directory listing. 


@subsection{Source files in the dashboard}

We see the only file, @racketfont{poem.html.pp}. Note that the @racketfont{.pp} extension is grayed out. The dashboard automatically consolidates references to source and output files into a single entry. What this entry says is ``The directory contains a source file in @racketfont{.pp} format for the output file @racketfont{poem.html}.''

Every source-file entry in the dashboard has three links. The first link is attached to the filename itself, and takes you to a preview of the output file. If the output file doesn't yet exist — as is the case here — it will be dynamically rendered. (This is true whether you click its name in the dashboard, or link to it from another page.) So click the filename. You'll see in your web browser:

@nested[#:style 'code-inset]{
The margin is 42em. The border is red. The padding is 15em. The border is too.} 

Granted, this is a boring web page. The main point here is that you're seeing the @italic{output} from your source file, which didn't exist before. Notice that the address bar says @racketfont{http://localhost:8080/poem.html}, not @racketfont{poem.html.pp}. And if you look in your @racketfont{tutorial} directory, you'll see a new file called @racketfont{poem.html}. 

In other words, when you clicked on the filename link in the dashboard, Pollen rendered the output file from your source file and saved it in your project directory. As promised earlier, the name of the output file (@racketfont{poem.html}) is the name of the source file (@racketfont{poem.html.pp}) minus the Pollen extension (@racketfont{.pp}).

If you go back to the dashboard and click on the filename link again, you'll see the same output file. If the source file hasn't changed, Pollen will just show you the output file that's already been rendered. 

But if you like, open your @racketfont{poem.html.pp} source file in DrRacket, edit the first two lines, and save the file:

@nested[#:style 'code-inset]{@verbatim{
#lang pollen

The cave is pitch black.
Look out for the grue.
The padding is 15em.
The border is too.}}

Go back to the dashboard and click on the filename. This time, you'll see:

@nested[#:style 'code-inset]{
The cave is pitch black. Look out for the grue. The padding is 15em. The border is too.} 

Here, Pollen notices that the source file has changed, so it refreshes the output file. This makes it convenient to work between DrRacket and your web browser, editing source and then reloading to see the changes.

The other two links in the dashboard are labeled @racketfont{in} and @racketfont{out}. 

The link labeled @racketfont{in} will display the contents of the source file:

@nested[#:style 'code-inset]{@verbatim{
#lang pollen

The cave is pitch black.
Look out for the grue.
The padding is 15em.
The border is too.}}

The link labeled @racketfont{out} will display the contents of the output file (just like the ``view source'' option in your web browser):

@nested[#:style 'code-inset]{@verbatim{
The cave is pitch black.
Look out for the grue.
The padding is 15em.
The border is too.}}

For now, the files are identical except for the @racketfont{#lang} line. But let's change that.

@section{Working with the preprocessor}

Pollen can operate in several processing modes. One of these is @italic{preprocessor} mode.  A preprocessor is a tool for making systematic, automated changes to a file, often in contemplation of further processing (hence the @italic{pre-}). You can use the Pollen preprocessor this way. Or you can just use it on its own, and leave your files in a finished state. 

That's how we'll use it in this tutorial. We'll build out our @racketfont{poem.html.pp} source file so that it exits the preprocessor as a legit HTML file.

@subsection{Setting up a preprocessor source file}

The file extension of a Pollen source file tells Pollen what kind of processing to apply to it. The ``@racketfont{.pp}'' file extension stands for ``Pollen preprocessor.'' You can use the preprocessor with any text-based file by:
@itemlist[

@item{inserting @racketfont{#lang pollen} as the first line,}

@item{adding the @racketfont{.pp} file extension,}

@item{running it through Pollen.}
]

@margin-note{For more about the Pollen processing modes and how to invoke them, see @secref["file-types"].}

``The preprocessor be used with @bold{any} kind of text-based file?'' Right. ``But how?'' The preprocessor reads the source file, handles any Pollen commands it finds, and lets the rest of the content pass through untouched. To the preprocessor, it's all just text data. It doesn't care whether that text represents HTML, CSS, JavaScript, or even @link["https://en.wikipedia.org/wiki/TI-BASIC"]{TI-BASIC}.

Because the preprocessor only deals in text, the Pollen commands you use in the preprocessor also have to produce text. Moreover, Pollen doesn't enforce the semantics of the underlying file — that's your responsibility. For instance, Pollen won't stop you from doing nonsensical things like this:

@filebox["bad-poem.html.pp"]{@verbatim{
#lang pollen

The cave is pitch black.
Look out for the grue.
◊(insert-mp3-recording-of-scream)
 }}

Here, the result is not going to be valid HTML, because you can't simply drop binary data in the middle of an HTML file. To paraphrase Mr. Babbage — garbage in, garbage out.

I've encouraged you to mess with the source file, but let's return it to its original state:

@filebox["/path/to/tutorial/poem.html.pp"]{@verbatim{
#lang pollen

The margin is 42em.
The border is red.
The padding is 15em.
The border is too.}}

This file has @racketfont{#lang pollen} as the first line, and @racketfont{.pp} as the file extension, so it meets the minimum requirements for the preprocessor.

@subsection{Creating valid HTML output}

Let's update our source so it produces valid HTML. Edit the source as follows:

@filebox["/path/to/tutorial/poem.html.pp"]{@verbatim{
#lang pollen
<!DOCTYPE html>
<html>
<body>
<pre>
The margin is 42em.
The border is red.
The padding is 15em.
The border is too.
</pre>
</body>
</html>}}

Return to the project server and view @link["http://localhost:8080/poem.html" "http://localhost:8080/poem.html"]. Earlier, the output looked like this:

@nested[#:style 'code-inset]{
The margin is 42em. The border is red. The padding is 15em. The border is too.} 


But now, because of the @racketfont{<pre>} tag, the poem will appear in a monospaced font, and the line breaks will be preserved:

@nested[#:style 'code-inset]{
@tt{The margin is 42em.
@(linebreak)The border is red.
@(linebreak)The padding is 15em.
@(linebreak)The border is too.}}

As before, because the source has changed, Pollen refreshes the output file. From the dashboard, you can use the @racketfont{in} and @racketfont{out} links to inspect the source and output. 

This is now a valid HTML page.

@subsection{Adding commands}

I mentioned that the preprocessor reads the file and handles any Pollen commands it finds. But our source file doesn't have any commands yet. Let's add some.

Pollen commands can be embedded in your source file using one of two modes: @italic{Racket mode} or @italic{text mode}. We'll try text mode in a later tutorial. For now, we'll use Racket mode.

To make a Racket-mode Pollen command, just take any Racket expression and put the lozenge character (@litchar["◊"]) in front of it. For instance, these are valid Racket expressions:

@nested[#:style 'code-inset]{@verbatim{
    (define inner 2)
    (define edge (* inner 4))
    (define color "blue")
}}

And these are the equivalent commands in Pollen:

@nested[#:style 'code-inset]{@verbatim{
    ◊(define inner 2)
    ◊(define edge (* inner 4))
    ◊(define color "blue")
}}

How to type a lozenge:
@(linebreak)@bold{Mac}: option + shift + V
@(linebreak)@bold{Windows}: holding down alt, type 9674 on the num pad
@(linebreak)@bold{Ubuntu}: ctrl + shift + U, then 25CA

@subsection{Racket basics (if you're not familiar)}

``But how am I supposed to know Racket?'' You don't. So we'll start now. Here are the five basic rules of Racket:

@itemlist[#:style 'ordered

@item{The core building block of Racket is the @italic{expression}. An expression can be a value (like @racket[2] or @racket{blue}), a variable (like @racketfont{edge}), or a function call (like @racket[(* inner 4)]).}

@item{Every expression is @italic{evaluated} to produce a value.}

@item{A variable evaluates to whatever value it holds (so @racketfont{inner} would become @racket[2]). A function call evaluates to its return value (so @racket[(+ 1 1)] would become @racket[2]).}

@item{Function calls go between parentheses. Unlike most languages, the function name comes @italic{first}, followed by its arguments (so it's @racket[(* inner 4)], not @racket[(inner * 4)]). This is called @italic{prefix notation}.}

@item{Expressions can contain recursively nested expressions. Thus, @racket[(* inner 4)] could be written @racket[(* inner (+ 2 2))] or @racket[(* inner (+ (+ 1 1) (+ 1 1)))].}

]

@margin-note{Newcomers to Racket often gripe about prefix notation and the parentheses. If you need to get it out of your system, go ahead. Keep in mind, however, that it's not some peculiar affectation, but rather a necessary consequence of rule #1. As you'll come to learn, rule #1 is where the magic happens.}

That should tell you enough to infer what's going on in the Pollen commands above:

@nested[#:style 'code-inset]{@verbatim{
    ◊(define inner 2)
    ◊; create a variable 'inner' that holds the value 2
    ◊(define edge (* inner 4))
    ◊; create a variable 'edge' that's four times the value of 'inner'
    ◊(define color "blue")
    ◊; create a variable 'color' that holds the value "blue"
}}

To learn more about Racket syntax, consider a detour through the excellent @other-doc['(lib "scribblings/quick/quick.scrbl")].


@subsection{Defining variables with commands}

Let's use commands to define variables that will hold some values for our page. First, add a @racketfont{<head>} tag to your source file, and three commmands to define three variables:

@filebox["/path/to/tutorial/poem.html.pp"]{@verbatim{
#lang pollen
<!DOCTYPE html>
<html>
<head>
◊(define inner 2)
◊(define edge (* inner 4))
◊(define color "blue")
</head>
<body>
<pre>
The margin is 42em.
The border is red.
The padding is 15em.
The border is too.
</pre>
</body>
</html>}}

Then look at @link["http://localhost:8080/poem.html" "http://localhost:8080/poem.html"] again. Does it look the same? Not a trick question — it should. If you click the @onscreen{Out} link on the dashboard, you'll see this:

@nested[#:style 'code-inset]{@verbatim{
<!DOCTYPE html>
<html>
<head>



</head>
<body>
<pre>
The margin is 42em.
The border is red.
The padding is 15em.
The border is too.
</pre>
</body>
</html>}}

What's happening here? Our @racketfont{◊(define ...)} commands just define variables, so they don't evaluate to any value. Instead, we get blank lines. So far, so good.

@subsection{Inserting values from variables}

To insert the value of a variable in our file, we use the command @litchar{◊|}@italic{variable-name}@litchar{|}. Let's do that now:

@filebox["/path/to/tutorial/poem.html.pp"]{@verbatim{
#lang pollen
<!DOCTYPE html>
<html>
<head>
◊(define inner 2)
◊(define edge (* inner 4))
◊(define color "blue")
</head>
<body>
<pre>
The margin is ◊|edge|em.
The border is ◊|color|.
The padding is ◊|inner|em.
The border is too.
</pre>
</body>
</html>}}

Here, we're replacing three values in the poem with the variables containing those values — @racketfont{◊|edge|}, @racketfont{◊|color|}, and @racketfont{◊|inner|}. @link["http://localhost:8080/poem.html"]{Reload the file} in the project server, and you'll see:

@nested[#:style 'code-inset]{@verbatim{
The margin is 8em.
The border is blue.
The padding is 2em.
The border is too.}}

Hey, look at that — the text of the poem changed. Now it even rhymes.

If you like, in the source file, edit the variable definitions with different values and reload the page in the project server. The page will be rendered afresh with the new values. In particular, if you update @racketfont{inner}, you'll also see @racketfont{edge} change, since its value depends on @racketfont{inner}.

@subsection{Inserting variables within CSS}

Our poem makes claims about the @racketfont{margin}, @racketfont{border}, and @racketfont{padding} of the page that aren't yet true. To fix this, we'll rely on the same basic technique of inserting variables into our HTML file. But instead of putting them in the @racketfont{<body>} of the page, we'll put them in a CSS @racketfont{<style>} tag.

Update the @racketfont{<head>} section of the page with a new @racketfont{<style>} tag that defines a style for @racketfont{pre} like so, using our variables for the relevant values:


@filebox["/path/to/tutorial/poem.html.pp"]{@verbatim{
#lang pollen
<!DOCTYPE html>
<html>
<head>
◊(define inner 2)
◊(define edge (* inner 4))
◊(define color "blue")
<style type="text/css">
pre {
    margin: ◊|edge|em;
    border: ◊|inner|em solid ◊|color|;
    padding: ◊|inner|em;
}
</style>
</head>
<body>
<pre>
The margin is ◊|edge|em.
The border is ◊|color|.
The padding is ◊|inner|em.
The border is too.
</pre>
</body>
</html>}}

Notice that we're using the same @litchar{◊|}@italic{variable-name}@litchar{|} pattern as before to insert the variable values. 

What do we expect to see? We expect that the @racketfont{padding} and @racketfont{border} will be 2em wide, because @racketfont{inner} is 2. We expect the @racketfont{margin} to be 8em, because it's equal to @racketfont{edge}, which is @racketfont{inner} multiplied by 4. And we expect the color of the border to be @racket["blue"], because that's the value of the variable @racketfont{color}.

And indeed, when you @link["http://localhost:8080/poem.html"]{reload the file} in the project server, you'll see exactly that:

@image["scribblings/result.png" #:scale 0.7]


As before, if you edit the values of the variables in the source file and reload in the project server, you'll see both the text and the layout change.


@section{First tutorial complete}

This was a sneaky tutorial. The HTML page we made was very simple, but in building it, we covered many important points about how Pollen works. 

Feel free to go back and experiment with what you've learned. The next tutorial will assume that you're comfortable with all the material here.






