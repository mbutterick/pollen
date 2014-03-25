#lang scribble/manual

@title{Quick start}



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

You can work with Pollen source files in any text editor. The key advantage of DrRacket is that you can run them too, and see if they work the way you expect.

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

@item{The @tt{render} command takes the result from your source file — meaning, the result you previewed in DrRacket in the previous step — and saves it to an output file.}

@item{The name of the output file is the same as the source file, minus the Pollen source extension. So @tt{hello.txt.pp} becomes @tt{hello.txt}.}
]

Try editing the text in the @tt{hello.txt.pp} source file and running @tt{raco pollen render hello.txt.pp} again. The old @tt{hello.txt} will be replaced with a new one showing your changes. And so you've learned a fourth thing:

@itemlist[
@item{Pollen works by rendering output files from source files. Output files can be overwritten. Therefore, you should only make edits to your source files.}
]


@section{Using the development server}

You've just learned two ways to see the output of a Pollen source file — first, you ran it in DrRacket. Then, you rendered it to an output file. 

Now here's a third: the Pollen development server.




@section{The plot thickens}

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


@section{Making an HTML page with Pollen}

By default, Pollen operates in preprocessor mode. That means it evaluates all the expressions in your document, renders each as text, and then outputs the whole document as a text file. 

In this tutorial, you're going to make an HTML file. But you can use Pollen as a preprocessor for any kind of text file.

@margin-note{That means Pollen can act as a preprocessor for CSS, JavaScript, XML — and even source files for other programming languages.}


@subsection{Making an HTML page with Pollen in decoder mode}

@subsection{Using the Pollen dashboard}




