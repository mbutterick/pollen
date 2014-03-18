#lang scribble/manual

@title{Quick start}



@section{You had me at Hello World}

Launch DrRacket. Open a new document.

Change the top line to #lang pollen. This will be the first line of any source file where you want to invoke the Pollen language.

Type @code{Hello World} underneath.

Run the file by clicking the Run button, or typing cmd-R.

The result window will show @code{Hello World}.

Congratulations, you've made your first Pollen document.

If you like, change the text and run the file again. The new text will appear in the result window.

Pollen is a programming language that operates in text mode by default. Meaning, all plain text in the source file is considered valid input, and gets passed through intact.

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




