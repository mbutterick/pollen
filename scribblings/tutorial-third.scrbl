#lang scribble/manual

@(require (for-label pollen/world))

@title[#:tag "tutorial-3"]{Third tutorial}

In this tutorial, you'll use Pollen to publish a multi-page article written in Markdown. You'll learn about:

@itemlist[

@item{Sharing data between source files}

@item{Meta and Doc}

@item(the project-require.rkt file)

@item{Pagetrees}

@item{Pagetrees with raco pollen render}



]


@subsection{Sharing data between preprocessor files}

The preprocessor is useful for inserting variables that hold values. But variables are only useful when they can be shared among multiple files. Let's look at one way to do that.

Any value in a Pollen file that's set up using @racket[define] can be pulled into another Pollen file using the @racket[require] function. For instance, let's set up another preprocessor file in the same directory as @racketfont["brennan.md.pp"], called @racketfont["dale.md.pp"]:

@filebox["dale.md.pp"]{@verbatim{
#lang pollen

My name is _Dale_, and I enjoy:

+ super-duper boring sauce

+ at least 3 fish nuggets}}

In the project server, this will produce the expected output:

@nested[#:style 'code-inset]{@verbatim{
My name is _Dale_, and I enjoy:

+ super-duper boring sauce

+ at least 3 fish nuggets}}

Now, let's update the content using values defined in @racketfont{brennan.md.pp}. We do this by adding the @racket[require] command to the top of our file:

@filebox["dale.md.pp"]{@verbatim{
#lang pollen

◊(require "brennan.md.pp")

My name is _Dale_, and I enjoy:

+ super-duper boring sauce

+ at least 3 fish nuggets}}

The three values that we defined in @racketfont{brennan.md.pp} — @racketfont{sauce-type}, @racketfont{nugget-type}, and @racketfont{nugget-quantity} — will now be available in @racketfont{dale.md.pp} under the same names, so we can insert them into the Markdown in the same way:

@filebox["dale.md.pp"]{@verbatim{
#lang pollen

◊(require "brennan.md.pp")

My name is _Dale_, and I enjoy:

+ super-duper ◊sauce-type sauce

+ at least ◊nugget-quantity ◊nugget-type nuggets}}

Reload the file in the project server, and you'll see the imported values:

@nested[#:style 'code-inset]{@verbatim{
My name is _Dale_, and I enjoy:

+ super-duper fancy sauce

+ at least 12 chicken nuggets}}


@margin-note{Those familiar with Racket know that Racket makes you explicitly @racket[provide] any variables you want to export. To make life simpler, Pollen inverts this behavior and automatically exports all defined symbols using @racket[(provide (all-defined-out))]. For more about the differences in behavior between Racket and Pollen, see @secref["File_formats" #:doc '(lib "pollen/scribblings/pollen.scrbl")].}








