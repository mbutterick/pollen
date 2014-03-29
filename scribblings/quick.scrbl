#lang scribble/manual

@title{Quick start}

@(define (link-tt url) (link url (tt url)))


@section{Creating a source file}

Assuming you've installed Racket & Pollen, launch DrRacket. 

Open a new document. Change the top line to:

@racketmod[pollen]

The first line of every Pollen source file will start with @tt{#lang pollen}.


@section{Running a source file}

Add a second line to your source file so it reads:

@racketmod[pollen
Hello world]

Click the @onscreen{Run} button. In the interactions window, you'll see the result:

@nested[#:style 'code-inset]{@racketvalfont{Hello world}}

Not bad. I think Pollen just won the @link["http://en.wikipedia.org/wiki/List_of_Hello_world_program_examples"]{Hello World Competition}.

You can work with Pollen source files in any text editor. The key advantage of DrRacket is that you can preview the results by running the file.

Try editing your source file:

@racketmod[pollen
Goodbye Stranger
Breakfast in America
Take the Long Way Home]

You don't have to use Supertramp song titles. Any text will do. When you click @onscreen{Run} again, you'll see whatever you typed:

@nested[#:style 'code-inset]{@racketvalfont{Goodbye Stranger}@(linebreak)@racketvalfont{Breakfast in America}@(linebreak)@racketvalfont{Take the Long Way Home}}

We won't do it a third time. You get the point — any plain text is valid within a Pollen source file, and gets printed as is. You never have to perform the incantations of typical programming languages:

@verbatim{
print "Hello world"
document.write('Hello world');
printf("Hello world");
}

In Pollen, what you write is what you get.


@section{Naming, saving, and rendering a source file}

Save this file with the name @tt{hello.txt.pp} in any convenient directory. The desktop is fine.

Open a terminal window and issue two commands:

@verbatim{
> cd [directory containing your file]
> raco pollen render hello.txt.pp}

After a moment, a new file will appear called @tt{hello.txt}. Let's see what's in it:

@verbatim{
> cat hello.txt
Goodbye Stranger
Breakfast in America
Take the Long Way Home
}

You've just learned three things:

@itemlist[

@item{Pollen commands in the terminal begin with @tt{raco pollen}, followed by a specific command (in this case @tt{render}) and sometimes an argument (in this case @tt{hello.txt.pp}).}

@item{The @tt{render} command takes the ouput from your source file — meaning, the result you previewed in DrRacket in the previous step — and saves it to another file.}

@item{The name of the output file is the same as the source file, minus the Pollen source extension. So @tt{hello.txt.pp} becomes @tt{hello.txt}.}
]

Try editing the text in the @tt{hello.txt.pp} source file and running @tt{raco pollen render hello.txt.pp} again. The old @tt{hello.txt} will be replaced with a new one showing your changes. And so you've learned a fourth thing:

@itemlist[
@item{Pollen works by rendering output files from source files. Output files can be overwritten. Therefore, you should only make edits to your source files.}
]


@section{The project server}

You've just learned two ways to see the output of a Pollen source file — first, you ran it in DrRacket. Then, you rendered it to an output file. 

Now here's a third: the Pollen project server. Here's how you start it. Return to your terminal window and issue two commands:

@verbatim{
> cd [directory containing your hello.txt.pp file]
> raco pollen start}

After a moment, you'll see the startup message:

@verbatim{
Welcome to Pollen 0.001 (Racket 6.0.0.5)
Project root is /path/to/your/directory
Project server is http://localhost:8080 (Ctrl-C to exit)
Project dashboard is http://localhost:8080/index.ptree
Ready to rock}

Open a web browser and point it at @link-tt{http://localhost:8080/index.ptree}. The top of the window will say @tt{Project root}. Below that will be a listing of the files in the directory. 

Among them will be @tt{hello.txt}, with a greyed-out @tt{.pp} extension. Click on it, and you'll be taken to @link-tt{http://localhost:8080/hello.txt}, where you'll see:

@verbatim{
Goodbye Stranger
Breakfast in America
Take the Long Way Home
}

That's the boring part. Here's the good part. Leave the project server running. Open your source file again in DrRacket and edit it as follows:

@racketmod[#:file "hello.txt.pp" pollen
Mean Street
Panama
Hear About It Later]

Go back to your web browser and reload @link-tt{http://localhost:8080/hello.txt}. Now you'll see this:

@verbatim{
Mean Street
Panama
Hear About It Later}

Notice what happened — the Pollen project server dynamically regenerated the output file (@tt{hello.txt}) from the source file (@tt{hello.txt.pp}) after you edited the source. If you like, try making some more changes to @tt{hello.txt.pp}, and reloading the browser to see the updates in @tt{hello.txt}.


@section{Intermission}

That covers input & output. Now let's circle back and look at what else you can do with Pollen (beyond the epic achievement of displaying plain text in a web browser).

For the rest of this tutorial, I recommend keeping two windows on screen: a web-browser window pointed at your project server (the main URL is @link-tt{http://localhost:8080/index.ptree}) and the DrRacket editing window.

@section{Pollen as a preprocessor}

A @italic{preprocessor} is a tool for making systematic, automated changes to a source file before the main processing happens. A preprocessor can also be used to add programming logic to files that otherwise don't support it.

For instance, HTML. In DrRacket, create a new file called @tt{margin.html.pp} in your project directory:

@racketmod[#:file "margin.html.pp" pollen
<body style="margin: 5em; border:1px solid black">
5em is the inset.
</body>]

The ``@tt{.pp}'' file extension — which you saw before, with @tt{hello.txt.pp} — stands for ``Pollen preprocessor.'' You can use the Pollen preprocessor with any text-based file by inserting @tt{#lang pollen} as the first line, and adding the @tt{.pp} file extension.

But for now, go to your @link["http://localhost:8080/index.ptree"]{project dashboard} and click on @link["http://localhost:8080/margin.html"]{@tt{margin.html}}. You should see a black box containing the text ``5em is the inset.''

Let's suppose you want to change the inset to 30%. Without a preprocessor, you'd have to search & replace each value. But with a preprocessor, you can move the inset value into a variable, and update it from that one location. So first, introduce a variable called @tt{my-inset} by using the @racket[define] command:

@racketmod[#:file "margin.html.pp" pollen
◊define[my-inset]{30%}
<body style="margin: 10em; border:1px solid black">
10em is the inset.
</body>
]

The ◊ character is called a @italic{lozenge}. In Pollen, the lozenge is a special character that marks anything Pollen should interpret as a command (rather than plain text). The whole command @tt{◊define[my-inset]{30%}} means ``create a variable called @tt{my-inset} and give it the value @tt{30%}.''

Then put the variable into the HTML like so, this time using the ◊ character with the variable name in the two places the value appears:

@racketmod[#:file "margin.html.pp" pollen
◊define[my-inset]{30%}
<body style="margin: ◊my-inset; border:1px solid black">
◊my-inset is the inset.
</body>
]

Now reload @link["http://localhost:8080/margin.html"]{@tt{margin.html}}. You'll see that the size of the margin has changed (because of the change to the @tt{style} attribute) and so has the text of the HTML. If you like, try editing @tt{my-inset} with different values and reloading the page. You can also try using @racket[define] to create another variable (for instance, to change the color of the box border).

Still, this is the tiniest tip of the iceberg. The Pollen preprocessor gives you access to everything in the Racket programming language — including math functions, text manipulation, and so on.

@section{Markdown mode}

When used as a preprocessor, Pollen's rule is that what you write is what you get. But if you're targeting HTML, who wants to type out all those @tt{<tedious>tags</tedious>}? You can make Pollen do the heavy lifting by using it as a source decoder.

For instance, Markdown mode. Markdown is a simplified @link["https://daringfireball.net/projects/markdown/"]{notation system} for HTML. You can use Pollen's Markdown decoder by inserting @tt{#lang pollen} as the first line, and adding the @tt{.pmd} file extension.

Try it. In DrRacket, create a file with the following lines and save it as @tt{downtown.html.pmd}:

@racketmod[#:file "downtown.html.pmd" pollen

Pollen + Markdown
-----------------

+ You **wanted** it — you #,(racketfont "_got_") it.

+ [search for Racket](https://google.com/search?q=racket)
]

As before, go to the @link["http://localhost:8080/index.ptree"]{dashboard} for the project server. This time, click the link for @link["http://localhost:8080/downtown.html"]{@tt{downtown.html}}. You'll see something like this:

@nested[#:style 'code-inset]{
@bold{@larger{Pollen + Markdown}}

@nested[#:style 'inset]{@itemlist[

@item{You @bold{wanted} it — you @italic{got} it.}

@item{@link["https://google.com/search?q=racket"]{search for Racket}}
]}}

As usual, you're welcome to edit @tt{downtown.html.pmd} and then refresh the web browser to see the changes.

In Markdown mode, you can still embed Pollen commands within the source as you did in preprocessor mode. Just keep in mind that your commands need to produce valid Markdown (as opposed to raw HTML). For instance, use @tt{define} to create a variable called @tt{metal}, and insert it into the Markdown:

@racketmod[#:file "downtown.html.pmd" pollen
◊define[metal]{Plutonium}
 
Pollen + ◊metal
--------
 
+ You **wanted** ◊metal — you #,(racketfont "_got_") it.
 
+ [search for ◊metal](https://google.com/search?q=◊metal)
]

Refresh @link["http://localhost:8080/downtown.html"]{@tt{downtown.html}} in the browser:

@nested[#:style 'code-inset]{
@bold{@larger{Pollen + Plutonium}}

@nested[#:style 'inset]{@itemlist[

@item{You @bold{wanted} Plutonium — you @italic{got} it.}

@item{@link["https://google.com/search?q=Plutonium"]{search for Plutonium}}
]}}

Pollen is handling two tasks here: interpreting the commands in the source, and then converting the Markdown to HTML. But what if you wanted to use Pollen as a preprocessor that outputs a Markdown file? No problem — just change the source name from @tt{downtown.html.pmd} to @tt{downtown.md.pp}. Changing the extension from @tt{.pmd} to @tt{.pp} switches Pollen from Markdown mode back to preprocessor mode. And changing the base name from @tt{downtown.html} to @tt{downtown.md} updates the name of the output file.


@section{Markup mode}

If all you need to do is produce basic HTML, Markdown is great. But if you need to do semantic markup or other kinds of custom markup, it's not flexible enough. 

In that case, you can use Pollen markup mode. To use Pollen markup, insert @tt{#lang pollen} as the first line of your source file, and add a @tt{.pm} file extension.

Compared to Markdown mode, Pollen markup mode is wide open. Markdown mode gives you a limited set of formatting tools (i.e., the ones supported by Markdown). But in markup mode, you can use any tags you want. Markdown mode decodes the source in a fixed way (i.e., with the Markdown decoder). But markup mode lets you build any decoder you want.

Let's convert our Markdown example into Pollen markup. Marking up content is simple: insert the lozenge character (@tt{◊}) followed by the name of the tag (@tt{◊tag}), followed by the content of the tag in curly braces (@tt{◊tag{content}}). In DrRacket, create a new file called @tt{uptown.html.pm} as follows:



@racketmod[#:file "uptown.html.pm" pollen

◊headline{Pollen markup}

◊items{

◊item{You ◊strong{wanted} it — you ◊em{got} it.} 

◊item{◊link["https://google.com/search?q=racket"]{search for Racket}}

}]

Go to the @link["http://localhost:8080/index.ptree"]{project dashboard} and click on @link["http://localhost:8080/uptown.html"]{@tt{uptown.html}}. You'll see something like this:

@nested[#:style 'code-inset]{
Pollen markup You @bold{wanted} it — you @italic{got} it. https://google.com/search?q=racketsearch for Racket}

That's not right. What happened?

We marked up the source using a combination of standard HTML tags (@tt{strong}, @tt{em}) and nonstandard ones (@tt{headline}, @tt{items}, @tt{item}, @tt{link}). This is valid Pollen markup. (In fact, if you look at @link["http://localhost:8080/out/markup.html"]{the generated source}, you'll see that they didn't disappear.) But since we're targeting HTML, we need to convert our custom tags into valid HTML tags.

For that, we'll make a special file called @tt{project-require.rkt}. This is a file in the standard Racket language that provides helper functions to decode the source. The definitions won't make sense yet. But this is the quick start, so all you need to do is copy, paste, and save:

@racketmod[#:file "project-require.rkt" racket/base
(require pollen/tag)
(provide (all-defined-out))
(define headline (make-tag-function 'h2))
(define items (make-tag-function 'ul))
(define item (make-tag-function 'li 'p))
(define (link url text) `(a [[href ,url]] ,text))
]

Return to the @link["http://localhost:8080/index.ptree"]{project dashboard} and click on @link["http://localhost:8080/uptown.html"]{@tt{uptown.html}}. Now you'll get the right result:

@nested[#:style 'code-inset]{
@bold{@larger{Pollen markup}}

@nested[#:style 'inset]{@itemlist[

@item{You @bold{wanted} it — you @italic{got} it.}

@item{@link["https://google.com/search?q=racket"]{search for Racket}}
]}}

Markup mode takes a little more effort to set up. But it also allows you more flexibility. If you want to do semantic markup, or convert your source into multiple output formats, or handle complex page layouts — it's the way to go. 

@section{Templates}

The HTML pages we just made looked pretty dull. For the last stop on the quick tour, let's fix that.

Pollen source files that are written in Markdown or markup mode (i.e., @tt{.pmd} or @tt{.pm} files) are rendered with a @italic{template}. A template is not a standalone Pollen source file. It's a file of the output type — e.g., CSS, HTML, XML — where you put the stuff that needs to be consistent between output files. The template also contains @italic{template variables} that mark where values from the Pollen source file should be inserted.

When it needs a template, Pollen first looks for a file in the project directory named @tt{template.[output extension of source]}. For @tt{uptown.html.pm}, the output extension will be @tt{.html}, thus Pollen will look for @tt{template.html}.

So let's create @tt{template.html}. Make a new file that with the following lines and save it to the same directory as @tt{uptown.html.pm}:

@filebox["template.html"]{@verbatim{<html><head><meta charset="UTF-8"/></head>
<body style="background: #f6f6f6">
<div style="background: white; margin: 3em; 
border:10px double gray; padding: 3em; font-size: 130%;">
This file is ◊here 
<hr />
◊->html{◊doc}
</div></body></html>
}}

This is a simple HTML file that should look familiar, except for the two template variables. The first, @tt{here}, contains the name of the current source file. As before, the lozenge character marks it as a Pollen command rather than text, and you write it as @tt{◊here}. The other command, @tt{◊->html{◊doc}}, takes the content from the source file, which is contained in a variable called @tt{doc}, and converts it to HTML with a Pollen function called @tt{->html}.

Go back to your web browser and reload @link["http://localhost:8080/uptown.html"]{@tt{uptown.html}}. (Or @link["http://localhost:8080/downtown.html"]{@tt{downtown.html}} — both will work.) The page will be rendered with the new @tt{template.html}. As before, you can edit the template or the source and the project server will dynamically update the output file. 

@section{PS for Scribble users}

Pollen can also be used as a dynamic preview server for Scribble files. From your terminal, do the following:

@verbatim{
> cd [directory containing your Scribble files]
> raco pollen start}

On the @link["http://localhost:8080/index.ptree"]{project dashboard}, you'll see your @tt{[filename].scrbl} files listed as @tt{[filename].html}. This may not represent the ultimate structure of your Scribble project — you may end up combining multiple Scribble source files into one HTML file, or making multiple HTML files from one Scribble source — but it's handy for checking your work as you go.



@section{The end of the beginning}

Now you've seen the key features of Pollen. What do you think?

@itemlist[

@item{@italic{``So it's like WordPress, but harder to use?''} I was a happy WordPress user for several years. If you need a blog, it's great. But the farther you get from blogs, the more it becomes like teaching an elephant to pirouette. And for those who like to solve problems with programming, PHP is, um, limited.}

@item{@italic{``What about pairing a Python template system and Python web server?''} Good idea. I even tried it. But Python template systems don't offer you Python — they offer you pidgin dialects that ain't very Pythonic. Also, Python's handing of markup-based data structures is cumbersome.}

@item{@italic{``Haven't you heard of Jekyll?''} Yes. If everything you need to write is expressible in Markdown, it's great. If you need more than that, you're stuck.}

@item{@italic{``Sounds a lot like LaTeX. Why not use that?''} Also a good idea. LaTeX gets a lot of things right. But it wasn't designed for web publishing.}


@item{@italic{``Eh, there are plenty of adequate options. Why should I learn a system written in Racket, which I've never used?''} A salient objection. It's also the question I asked myself before I committed to Racket. But publishing systems that are author- or designer-friendly tend to be programmer-hostile, and vice versa. Racket is the only language I found that could handle every mode with ease.}


]

Don't take my word for it. The rest of this documentation will show you the cool, useful, and sophisticated things you can do with Pollen. If there's another tool that still suits you better, great. But I didn't make Pollen because I'm a programmer. I'm a writer who wants to make electronic books that are better than the ones we have now.  And for that, I needed a better tool.